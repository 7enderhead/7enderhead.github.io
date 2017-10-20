#lang racket
(require db)

(define con
  (virtual-connection
   (connection-pool
    (lambda () (mysql-connect #:server "127.0.0.1"
                              #:user "root"
                              #:password "MySql123"
                              #:database "test1")))))

(define (all-persons)
  (query-rows con "select * from person"))

(provide all-persons)