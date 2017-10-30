#lang racket

(require db)
(require setup/getinfo)
(require "structs.rkt")

(define connection
  (let ([info (get-info/full ".")])
    (virtual-connection
     (connection-pool
      (lambda () (mysql-connect #:server (info 'db-server)
                                #:user (info 'db-user)
                                #:password (info 'db-password)
                                #:database (info 'db-name)))))))

(define (stops)
  (for/list ([row (query-rows connection "select * from stop")])
    (apply stop (vector->list row))))

(define groups (group-by (lambda (stop) (stop-name stop)) (stops)))
(define group (first groups))
(for*/list ([s1 group]
            [s2 group]
            #:when (<= (stop-distance s1 s2) 5))
  (list s1 s2  (stop-distance s1 s2)))

(provide (all-defined-out))