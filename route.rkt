#lang racket

(require racket/struct)

(struct route (id type number start end)
  #:transparent
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (r) 'route)
      (lambda (r) (list (route-id r)
                        (route-type r)
                        (route-number r)
                        (route-start r)
                        (route-end r)))))])

(provide (all-defined-out))