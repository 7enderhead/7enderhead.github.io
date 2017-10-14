#lang racket
(require db)

(define con
  (virtual-connection
   (connection-pool
    (lambda () (mysql-connect #:server "192.168.0.24"
                              #:user "root"
                              #:password "MySql123"
                              #:database "test1")))))

(define (all-persons)
  (query-rows con "select * from person"))

(provide all-persons)