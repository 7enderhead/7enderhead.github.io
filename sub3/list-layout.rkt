#lang racket

(require racket/struct)
(require "data-defs.rkt")
(require "util.rkt")

(struct list-layout (filter-expr min-lon max-lon min-lat max-lat sorting)
  #:transparent
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (l) 'list-layout)
      (lambda (l) (list (list-layout-filter-expr l)
                        (list-layout-min-lon l)
                        (list-layout-max-lon l)
                        (list-layout-min-lat l)
                        (list-layout-max-lat l)
                        (list-layout-sorting l)))))])

(define (filter-stops stops list-layout)
  (filter
   (lambda (stop)
     (and (filter-expr-match?
           (format "(?i:~a)" (list-layout-filter-expr list-layout))
           (name stop))
          (>= (car (lon-range stop)) (list-layout-min-lon list-layout))
          (<= (cdr (lon-range stop)) (list-layout-max-lon list-layout))
          (>= (car (lat-range stop)) (list-layout-min-lat list-layout))
          (<= (cdr (lat-range stop)) (list-layout-max-lat list-layout))))
   stops))

(provide (all-defined-out))