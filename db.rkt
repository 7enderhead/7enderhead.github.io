#lang racket

(require db)
(require setup/getinfo)
(require "stop.rkt")

(define info (get-info/full "."))

(define connection
  (virtual-connection
   (connection-pool
    (lambda () (mysql-connect #:server (info 'db-server)
                              #:user (info 'db-user)
                              #:password (info 'db-password)
                              #:database (info 'db-name))))))

(define (stops)
  (for/list ([row (query-rows connection "select * from stop")])
    (apply stop (vector->list row))))

(define groups (group-by (lambda (stop) (stop-name stop)) (stops)))
(define group (first groups))
(for*/list ([s1 group]
            [s2 group]
            #:unless (equal? s1 s2)
            #:when (<= (distance s1 s2) (info 'compound-distance)))
  (list s1 s2 (distance s1 s2)))

(provide (all-defined-out))