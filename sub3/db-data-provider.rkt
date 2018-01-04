#lang scribble/lp2

@defmodule["db-data-provider.rkt"]

@title[#:tag "db-data-provider"]{Database-Backed Data Provider db-data-provider%}

@section{Data Model}

@itemlist[
 @item{database is modelled according to the following ER diagram:}
 @item{(note that the food table is not used yet as no restaurant data is needed in task 2)}
 ]

@image["./doc/er-diagram.png"]

@section{Class db-data-provider%}

@defclass[db-data-provider% object% (data-provider<%>)]{

 Implements @racket[data-provider<%>] via a MySQL database backend:

 @itemlist[
 @item{manages a virtual connection pool to the database}
 @item{implements all interface messages via queries on the connection}
 ]
                                     
}

@chunk[<db-data-provider%>
       (define db-data-provider%
         (class* object% (data-provider<%>)

           (init server user password database poll-millisecs)

           (super-new)

           <connection-handling>

           <stop-handling>

           <route-handling>

           <route-update-handling>

           <food-handling>
           ))]

@section{Connection Handling}

@itemlist[
 @item{virtual connection pool scales to multiple clients and makes this provider usable with, e.g., web-clients}
 @item{see the documentation at @racket[virtual-connection]}
 ]



@chunk[<connection-handling>
       (define connection
         (virtual-connection
          (connection-pool
           (lambda () (mysql-connect #:server server
                                     #:user user
                                     #:password password
                                     #:database database)))))]

@section{Stop Handling}

@chunk[<stop-handling>
       <stops-caching>

       <stops>

       <stops-by-id>]

@subsection{Stops Caching}

@itemlist[
 @item{stops never change in the database}
 @item{remember stops in (lazily initialized) top level variable cache}
 ]

@chunk[<stops-caching>
       (define all-stops #f)]

@subsection[#:tag "query-all-stops"]{Query All Stops}

selected with the following simple SQL statement:

@chunk[<stops-select-statement>
       "select id,lon,lat,name,alt_name from stop"]

@itemlist[
 @item{database layer returns a @racket[vector] with the queried fields in order}
 @item{convert to @racket[stop] structure}
 ]

@chunk[<stops>
       (define/public (stops)
         (unless all-stops
           (set! all-stops
                 (for/list ([row (query-rows connection <stops-select-statement>)])
                   (apply stop (vector->list row)))))
         all-stops)]

@chunk[<stops-by-id>
       (define all-stops-by-id #f)
    
       (define/public (stops-by-id)
         (when (not all-stops-by-id)
           (set! all-stops-by-id (group-stops-by-id (send this stops))))
         all-stops-by-id)]

@section{Route Handling}

@chunk[<route-handling>
       <route-caching>
    
       <route-querying>
    
       <route-insertion>]

@subsection{Route Caching}

@itemlist[
 @item{routes may change because of updates}
 @item{provide cache clearing via @racket[reset-routes]}
 ]

@chunk[<route-caching>
       (define all-routes #f)

       (define (reset-routes)
         (set! all-routes #f))]

@subsection{Route Querying}

@chunk[<route-querying>
       <all-routes>
    
       <routes-for-stop>

       <route-exists?>]

@subsubsection{Query All Routes}

@itemlist[
 @item{very similar to @seclink["query-all-stops"]{querying all stops}}
 ]

@chunk[<all-routes>
       (define/public (routes)
         (unless all-routes
           (set! all-routes
                 (for/list ([row (query-rows connection "select id,number,type,start,end from route")])
                   (apply route (vector->list row)))))
         all-routes)]

@subsubsection{Routes for Stop}

@itemlist[
 @item{first query all route ids from the @italic{mapping} table for the given stop-id with the following SQL statement ('~a' is the placeholder for the id, which is formatted in):}]
 
@chunk[<query-mapped-routes-statement>
       "select * from mapping where stop_id = ~a"]

@chunk[<routes-for-stop-id>
       (define/public (routes-for-stop stop-id)
         (let* ([statement (virtual-statement
                            (format <query-mapped-routes-statement> stop-id))]
                [route-ids (for/list ([mapping (query-rows connection statement)])
                             (match-let ([(vector route-id _) mapping])
                               route-id))])
           (apply routes-for-ids route-ids)))]

@itemlist[
 @item{then query for all these routes in one step in @racket[routes-for-ids]}
 ]

@chunk[<routes-for-stop>
       <routes-for-stop-id>

       (define (routes-for-ids . ids)
         (if (empty? ids)
             '()
             (let* ([id-list (string-join (map ~a ids) ",")]
                    [statement (virtual-statement
                                (format "select * from route where id in (~a)" id-list))]
                    [route-data (query-rows connection statement)])
               (for/list ([route-datum route-data])
                 (apply route (vector->list route-datum))))))]

@subsubsection{Does Route Exist?}

@itemlist[
 @item{takes an already created @racket[route] struct}
 @item{checks for existence of route with the same data, i.e., ignoring @racket[route-id]}
 ]

@chunk[<route-exists?>
       (define/public (route-exists? route)
         (if (findf (lambda (existent)
                      (and
                       (equal? (route-type existent) (route-type route))
                       (equal? (route-number existent) (route-number route))
                       (equal? (route-start existent) (route-start route))
                       (equal? (route-end existent) (route-end route))))
                    (send this routes))
             #t
             #f))]

@subsection{Route Insertion}

@itemlist[
 @item{route data is taken from given @racket[route] struct (whose @racket[route-id] is ignored)}
 @item{route insertion is straightforward with a plain SQL statement}
 ]

@chunk[<route-insertion-statement>
       "insert into route(number,type,start,end) values('~a','~a','~a','~a')"]

@itemlist[
 @item{@racket[format-route-data] helps to put the route's data into the correct SQL statement places:}
 ]

@chunk[<format-route-data>
       (define (format-route-data format-string route)
         (format format-string
                 (route-number route)
                 (route-type route)
                 (route-start route)
                 (route-end route)))]

@itemlist[
 @item{after insertion, the routes cache is reset (because we know an update happened)}
 @item{if given, any stops are inserted for the new route}
 ]

@chunk[<route-insertion>
       (define/public (insert-route route [stop-ids null])
         (unless (route-exists? route)
           (let ([insert-statement
                  (format-route-data <route-insertion-statement> route)])
             (query-exec connection insert-statement)
             (reset-routes))
           (insert-route-stops route stop-ids)))

       <format-route-data>
    
       <insert-route-stops>]

@subsubsection{Stop Insertion for Given Route}

@itemlist[
 @item{to insert stops we first find out which database id the newly inserted route has:}
 ]

@chunk[<get-id-for-route-data-statement>
       "select id from route where number='~a' and type='~a' and start='~a' and end='~a'"]

@itemlist[
 @item{then insert (route-id, stop-id) pairs for each given stop-id}
 @item{insertion is done in one statement with all pairs given at once}
 ]

@chunk[<insert-route-stops>
       (define/public (insert-route-stops route stop-ids)
         (let* ([id-statement
                 (format-route-data <get-id-for-route-data-statement> route)]
                [new-route-id (query-value connection id-statement)]
                [id-pairs (string-join (for*/list 
                                           ([route-id (list new-route-id)]
                                            [stop-id stop-ids])
                                         (format "(~a, ~a)" route-id stop-id))
                                       ",")]
                [insert-statement
                 (format "insert into mapping(route_id,stop_id) values ~a" id-pairs)])
           (query-exec connection insert-statement)))]

@section{Route Update Handling}

@chunk[<route-update-handling>
       <checksum>

       <callback-handling>
       
       <periodic-checksum-checking>]

@subsection{Table Checksum}

@itemlist[
 @item{each MySQL table has a checksum which is updated if the table is modified}
 @item{remember the checksum which corresponds to the currently cached @racket[all-routes] variable}
 ]

@chunk[<checksum>
       (define (query-checksum)
         (~> (query-row connection "checksum table route")
             (vector-ref 1)
             (->int)))

       (define checksum (query-checksum))]

@subsection{Handle Callbacks}

@itemlist[
 @item{register callback functions into a top-level variable}
 @item{@racket[invoke-callbacks] can be used to notify about updates}
 ]

@chunk[<callback-handling>
       (define callbacks (mutable-set))

       (define/public (add-callback callback)
         (set-add! callbacks callback))

       (define (invoke-callbacks)
         (set-for-each callbacks
                       (lambda (callback) (callback))))]

@subsection{Periodic Checksum Update}

@itemlist[
 @item{use a periodic timer which queries the current checksum}
 @item{on changes
  @itemlist[
 @item{update checksum data}
 @item{clear routes cache variable}
 @item{invoke registered callbacks}
 ]}
 ]

@chunk[<periodic-checksum-checking>
       (define timer
         (new timer%
              [interval poll-millisecs]
              [notify-callback
               (lambda ()
                 (let ([new-checksum (query-checksum)])
                   (unless (equal? new-checksum checksum)
                     (set! checksum new-checksum)
                     (reset-routes)
                     (invoke-callbacks))))]))]

@section{Food Places}

@chunk[<food-handling>
       (define all-food #f)

       (define/public (foods)
         (unless all-food
           (set! all-food
                 (for/list ([row (query-rows connection "select id,lon,lat,name,amenity,website,wheelchair,smoking,cuisine,opening_hours,outdoor_seating from food")])
                   (apply food (vector->list row)))))
         all-food)
       
       (define/public (food-at-place lon lat max-distance)
         (sort (for/list ([food-info (map (λ (f)
                                            (cons (hav:distance-in-meters-deg lon
                                                                              lat
                                                                              (food-lon f)
                                                                              (food-lat f))
                                                  f))
                                          (foods))]
                          #:when (<= (car food-info) max-distance))
                 food-info)
               (λ (a b) (< (car a) (car b)))))]

@section{File Structure}

@chunk[<*>
       <requires>
       
       <db-data-provider%>

       (provide db-data-provider%)
       ]

@chunk[<requires>
       (require racket)
       (require db)
       (require setup/getinfo)
       (require racket/format)
       (require threading)
       (require sugar/coerce)
       (require racket/gui/base) ; for timer%
       (require "data-provider.rkt")
       (require "data-defs.rkt")
       (require (prefix-in hav: "haversine.rkt"))]