#lang racket

(require racket/struct)

(struct stop (id lon lat name alt-name)
  #:transparent
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (s) 'stop)
      (lambda (s) (list (stop-id s)
                        (stop-lon s)
                        (stop-lat s)
                        (stop-name s)
                        (stop-alt-name s)))))])

(provide (all-defined-out))