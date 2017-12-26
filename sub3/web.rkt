#lang web-server/insta

(require racket/format)
(require web-server/http/bindings)
(require web-server/formlets)
(require web-server/formlets/input)
(require web-server/formlets/lib)
(require anaphoric)
(require threading)
(require setup/getinfo)
(require sugar/coerce)
(require "data-defs.rkt")
(require "data-provider.rkt")
(require "data-provider-factory.rkt")

;; debug imports
(require web-server/http/request-structs)

(define info (get-info/full "."))

(define provider (data-provider))

(define stops (~> (send provider stops)
                  (sort (lambda (stop1 stop2)
                          (string<? (stop-name stop1)
                                    (stop-name stop2))))))

(define (start request)
  (render-stop-info-page request))

(define (stop-list-formlet
         [in-stop1 #f]
         [in-use-name-filter1? #t]
         [in-name-filter1 ""])
  (printf "stop-list-formlet in-stop1: ~v, in-use-name-filter1?: ~v, in-name-filter1: ~v~n" in-stop1 in-use-name-filter1? in-name-filter1)
  (formlet
   (#%#
    (h2 ,(if in-stop1
             (stop-name in-stop1)
             "no stop selected"))
    (div ,{(multiselect-input stops
                              #:multiple? #f
                              #:attributes (list
                                            (list 'size
                                                  (->string (info 'web-stop-list-size))))
                              #:display (lambda (stop)
                                          (stop-name stop))) . => . stop1})
    (div ,{(cross (pure (lambda (x) (and x #t)))
                  (checkbox "" in-use-name-filter1?)) . => . use-name-filter1?}
         "Name filter "
         ,{(to-string (text-input #:value in-name-filter1)) . => . name-filter1})
    (div ,{(input #:type "range") . => . min-lon}))
   (values stop1 use-name-filter1? name-filter1 min-lon)))

(define (has-bindings? request)
  (not (null? (force (request-bindings/raw-promise request)))))

(define (render-stop-info-page request)
  (printf "request bindings: ~v~n" (force (request-bindings/raw-promise request)))
  (define-values (stop1-in use-name-filter1? name-filter1 min-lon)
    (with-handlers ([(lambda (e) #t) (lambda (e) (values #f #t #f #f))])
      (formlet-process (stop-list-formlet) request)))
  (define stop1 (if (and stop1-in (not (null? stop1-in)))
                    (car stop1-in)
                    #f))
  (printf "stop1: ~v, use-name-filter?: ~v, name-filter: ~v~n" stop1 use-name-filter1? name-filter1)
  (define (response-generator embed/url)
    (response/xexpr
     `(html
       (head (title "Stop Info"))
       (body (h1 "List of Stops")
             #;(h2 ,(if stop1
                        (stop-name stop1)
                        "no stop selected"))
             (form ([action ,(embed/url render-stop-info-page)])
                   ,@(formlet-display (stop-list-formlet stop1
                                                         use-name-filter1?
                                                         name-filter1))
                   (p (input ([type "submit"]))))
             ))))
  (send/suspend/dispatch response-generator))