#lang web-server/insta

(require racket/format)
(require web-server/http/bindings)
(require web-server/formlets)
(require web-server/formlets/input)
(require web-server/formlets/input)
(require anaphoric)
(require threading)
(require "data-defs.rkt")
(require "data-provider.rkt")
(require "data-provider-factory.rkt")

;; debug imports
(require web-server/http/request-structs)


(define provider (data-provider))

(define stops (~> (send provider stops)
                  (sort (lambda (stop1 stop2)
                          (string<? (stop-name stop1)
                                    (stop-name stop2))))))

(define (wrap-select stops)
  (for/list ([stop stops])
    `(option ([value ,(~a (stop-id stop))])
             ,(stop-name stop))))

(define (all-stops)
  (wrap-select stops))

(define (start request)
  (render-stop-info-page request))

(define stop-list-formlet
  (formlet
   (#%# (div ,{(select-input stops
                             #:attributes '((size "40"))
                             #:display (lambda (stop)
                                         (stop-name stop))) . => . stop1})
        (div ,{(checkbox "" #t) . => . use-name-filter?}
             "Name filter "
             ,{(text-input) . => . name-filter})
        (div ,{(input #:type "range") . => . min-lon}))
   (values stop1 use-name-filter? name-filter min-lon)))

(define (has-bindings? request)
  (not (null? (force (request-bindings/raw-promise request)))))

(define (render-stop-info-page request)
  (define-values (stop1 use-name-filter? name-filter min-lon)
    (with-handlers ([(lambda (e) #t) (lambda (e) (values #f #f #f #f))])
      (formlet-process stop-list-formlet request)))
  (printf "stop1: ~a, use-name-filter?: ~a, name-filter: ~a~n" stop1 use-name-filter? name-filter)
  (define (response-generator embed/url)
    (response/xexpr
     `(html
       (head (title "Stop Info"))
       (body (h1 "List of Stops")
             (h2 ,(if stop1
                      (stop-name stop1)
                      "no stop selected"))
             (form ([action ,(embed/url render-stop-info-page)])
                   ,@(formlet-display stop-list-formlet)
                   (p (input ([type "submit"]))))
             ))))
  (send/suspend/dispatch response-generator))