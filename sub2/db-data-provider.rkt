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
              (for/list ([row (query-rows connection "select * from stop")])
                (apply stop (vector->list row)))))
      all-stops)

    (define all-stops-by-id #f)
    
    (define/public (stops-by-id)
      (when (not all-stops-by-id)
        (set! all-stops-by-id (group-stops-by-id all-stops)))
      all-stops-by-id)

    (define all-routes #f)

    (define/public (routes)
      (unless all-routes
        (set! all-routes
              (for/list ([row (query-rows connection "select * from route")])
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
    
    ))

(provide db-data-provider%)