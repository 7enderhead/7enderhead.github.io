#lang racket

(require db)
(require setup/getinfo)
(require racket/format)
(require "data-provider.rkt")
(require "data-defs.rkt")

(define db-data-provider%
  (class* object% (data-provider<%>)

    (init server user password database)

    (super-new)

    (define connection
      (virtual-connection
       (connection-pool
        (lambda () (mysql-connect #:server server
                                  #:user user
                                  #:password password
                                  #:database database)))))

    (define all-stops #f)

    (define/public (stops)
      (unless all-stops
        (set! all-stops
              (for/list ([row (query-rows connection "select id,lon,lat,name,alt_name from stop")])
                (apply stop (vector->list row)))))
      all-stops)

    (define all-stops-by-id #f)
    
    (define/public (stops-by-id)
      (when (not all-stops-by-id)
        (set! all-stops-by-id (group-stops-by-id (send this stops))))
      all-stops-by-id)

    (define all-routes #f)

    (define/public (routes)
      (unless all-routes
        (set! all-routes
              (for/list ([row (query-rows connection "select id,number,type,start,end from route")])
                (apply route (vector->list row)))))
      all-routes)
    
    (define/public (routes-for-stop stop-id)
      (let* ([statement (virtual-statement
                         (format "select * from mapping where stop_id = ~a" stop-id))]
             [route-ids (for/list ([mapping (query-rows connection statement)])
                          (match-let ([(vector route-id _) mapping])
                            route-id))])
        (apply routes-for-ids route-ids)))

    (define (routes-for-ids . ids)
      (if (empty? ids)
          '()
          (let* ([id-list (string-join (map ~a ids) ",")]
                 [statement (virtual-statement
                             (format "select * from route where id in (~a)" id-list))]
                 [route-data (query-rows connection statement)])
            (for/list ([route-datum route-data])
              (apply route (vector->list route-datum))))))

    (define/public (route-exists? route)
      (findf (lambda (existent)
               (and
                (equal? (route-type existent) (route-type route))
                (equal? (route-number existent) (route-number route))
                (equal? (route-start existent) (route-start route))
                (equal? (route-end existent) (route-end route))))
             (send this routes)))
    
    (define/public (insert-route route stops)
      (unless (route-exists? route)
        (let ([insert-statement
               (format
                "insert into route(number,type,start,end) values('~a','~a','~a','~a')"
                (route-number route) (route-type route) (route-start route) (route-end route))])
          (query-exec connection insert-statement)
          (set! all-routes #f))
        (let* ([statement
                (format
                 "select id from route where number='~a' and type='~a' and start='~a' and end='~a'"
                 (route-number route) (route-type route) (route-start route) (route-end route))]
               [new-route-id (query-value connection statement)])
          new-route-id)))
    
    ))

(provide db-data-provider%)