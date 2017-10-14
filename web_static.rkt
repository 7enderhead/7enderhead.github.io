#lang web-server/insta

(require racket/format)
(require web-server/http/bindings)
(require anaphoric)
(require (prefix-in db: "db_test.rkt"))

(define (wrap-select persons)
  (for/list ([person persons])
    `(option ([value ,(~a (vector-ref person 0))])
             ,(vector-ref person 1))))

(define (all-persons)
  (wrap-select (db:all-persons)))

(define (start request)
  (show-all request))

(define (show-all request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html
       (head (title "All"))
       (body (h1 "List of Persons")
             (form ([action ,(embed/url show-selected)])
                   (p (select ([size "5"]
                               [name "person"])
                              ,@(all-persons)))
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
             (a ([href ,(embed/url show-all)])
                "Back to All")))))
  (send/suspend/dispatch response-generator))

(define (get-selected-person request)
  (let ([bindings (request-bindings request)])
    (if (exists-binding? 'person bindings)
        (extract-binding/single 'person bindings)
        #f)))