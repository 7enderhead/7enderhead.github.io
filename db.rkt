#lang racket

(require db)
(require setup/getinfo)
(require "structs.rkt")
(require "haversine.rkt")

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

(provide (all-defined-out))