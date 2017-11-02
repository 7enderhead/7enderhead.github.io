#lang racket

(require db)
(require setup/getinfo)
(require racket/format)
(require "data-defs.rkt")

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

(define (routes-for-stop stop-id)
  (let* ([statement (virtual-statement
                     (format "select * from mapping where stop_id = ~a" stop-id))]
         [route-ids (for/list ([mapping (query-rows connection statement)])
                      (match-let ([(vector route-id _) mapping])
                        route-id))])
    (apply routes route-ids)))

(define (routes . ids)
  (if (empty? ids)
      '()
      (let* ([id-list (string-join (map ~a ids) ",")]
             [statement (virtual-statement
                         (format "select * from route where id in (~a)" id-list))]
             [route-data (query-rows connection statement)])
        (for/list ([route-datum route-data])
          (apply route (vector->list route-datum))))))

#|
; tests for grouping stops with same name and given maximum distance
(define groups (group-by (lambda (stop) (stop-name stop)) (stops)))
(define group (first groups))
(for*/list ([s1 group]
            [s2 group]
            #:unless (equal? s1 s2)
            #:when (<= (distance s1 s2) (info 'compound-distance)))
  (list s1 s2 (distance s1 s2)))
|#

(provide (all-defined-out))