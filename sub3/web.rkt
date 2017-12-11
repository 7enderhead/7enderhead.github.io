#lang web-server/insta

(require racket/format)
(require web-server/http/bindings)
(require web-server/formlets)
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
   (div "Stop 1:" ,{select-input stops . => . stop1 })
   (list stop1)))

(define (render-stop-info-page request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html
       (head (title "Stop Info"))
       (body (h1 "List of Stops")
             (form ([action ,(embed/url show-selected)])
                   ,@(formlet-display stop-list-formlet)
                   (p (select ([size "40"]
                               [name "stop2"])
                              ,@(all-stops)))
                   (p (input ([type "submit"]))))))))
  (send/suspend/dispatch response-generator))

(define (render-stop-info-page_old request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html
       (head (title "Stop Info"))
       (body (h1 "List of Stops")
             (form ([action ,(embed/url show-selected)])
                   (p (select ([size "40"]
                               [name "stop1"])
                              ,@(all-stops)))
                   (p (select ([size "40"]
                               [name "stop2"])
                              ,@(all-stops)))
                   (p (input ([type "submit"]))))))))
  (send/suspend/dispatch response-generator))

(define (show-selected request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html
       (head (title "Selected"))
       (body (h1 "Selected Person")
             (p ,(if-let [selected (get-selected-person request)]
                         selected
                         "none selected"))
             (a ([href ,(embed/url render-stop-info-page)])
                "Back to All")))))
  (send/suspend/dispatch response-generator))

(define (get-selected-person request)
  (let ([bindings (request-bindings request)])
    (if (exists-binding? 'person bindings)
        (extract-binding/single 'person bindings)
        #f)))