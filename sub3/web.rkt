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
   (#%# ,{(select-input stops
                        #:attributes '((size "40"))
                        #:display (lambda (stop)
                                    (stop-name stop))) . => . stop1}
        ,{(text-input) . => . name-filter}
        ,{(input #:type "range") . => . min-lon}
       )
   (values stop1 name-filter min-lon)))

(define (render-stop-info-page request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html
       (head (title "Stop Info"))
       (body (h1 "List of Stops")
             (form ([action ,(embed/url show-selected-stop-info)])
                   ,@(formlet-display stop-list-formlet)
                   (p (input ([type "submit"]))))))))
  (send/suspend/dispatch response-generator))

(define (show-selected-stop-info request)
  (define-values (stop1 name-filter min-lon)
    (formlet-process stop-list-formlet request))
  (response/xexpr
   `(html
     (head (title "Detailed Info"))
     (body (h1 ,(stop-name stop1))
           ,(~a (binding:form-value name-filter)) ,(~a (binding:form-value min-lon))))))