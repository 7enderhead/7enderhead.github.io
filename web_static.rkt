#lang web-server/insta

(require "db_test.rkt")

(define (person-list-items)
  (for/list ([person (all-persons)])
    `(option ,(vector-ref person 1))))

(define (start request)
  (response/xexpr
   `(html
     (head (title "Static Web Page"))
     (body (h1 "List of Persons")
           (select ([size "5"]) ,@(person-list-items))))))
