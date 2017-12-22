#lang web-server/insta

(require racket/format)
(require web-server/http/bindings)
(require web-server/formlets)
(require web-server/formlets/input)
(require web-server/formlets/lib)
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

(define (start request)
  (render-stop-info-page request))

(define stop-list-formlet
  (formlet
   (#%# (div ,{(multiselect-input stops
                                  #:multiple? #f
                                  #:attributes '((size "40"))
                                  #:display (lambda (stop)
                                              (stop-name stop))) . => . stop1})
        (div ,{(cross (pure (lambda (x) (and x #t)))
                      (checkbox "" #t)) . => . use-name-filter?}
             "Name filter "
             ,{input-string . => . name-filter})
        (div ,{(input #:type "range") . => . min-lon}))
   (values stop1 use-name-filter? name-filter min-lon)))

(define (has-bindings? request)
  (not (null? (force (request-bindings/raw-promise request)))))

(define (render-stop-info-page request)
  (printf "request bindings: ~v~n" (force (request-bindings/raw-promise request)))
  (define-values (stop1-in use-name-filter? name-filter min-lon)
    (with-handlers ([(lambda (e) #t) (lambda (e) (values #f #f #f #f))])
      (formlet-process stop-list-formlet request)))
  (define stop1 (if (and stop1-in (not (null? stop1-in)))
                    (car stop1-in)
                    #f))
  (printf "stop1: ~v, use-name-filter?: ~v, name-filter: ~v~n" stop1 use-name-filter? name-filter)
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