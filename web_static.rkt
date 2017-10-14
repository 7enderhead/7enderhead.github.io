#lang web-server/insta

(require racket/format)
(require web-server/formlets)
(require "db_test.rkt")

(define (person-list-items)
  (for/list ([person (all-persons)])
    `(option ([value ,(~a (vector-ref person 0))])
             ,(vector-ref person 1))))

(define (render-persons [selected-person #f])
  (response/xexpr
   `(html
     (head (title "Static Web Page"))
     (body (h1 "List of Persons")
           (form 
            (select ([size "5"]
                    [name "person"])
                   ,@(person-list-items)))))))

(define (start request)
  (render-persons))