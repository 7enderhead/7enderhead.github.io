#lang racket

(require racket/gui/base)
(require racket/format) ; for ~a
(require "structs.rkt")
(require (prefix-in db: "db.rkt"))

(define (stops) (db:stops))

(define main-frame (new frame% [label "Stops"]
                   [width 400]
                   [height 800]))
 
(define (create-stop-list parent)
  (new list-box%
       [label "Stop:"]
       [parent parent]
       [choices '()]
       [columns '("Name")]
       [style '(single column-headers)]))

(define (create-filter-field parent list)
  (new text-field%
       [label "Filter:"]
       [parent parent]
       [callback (lambda (field event)
                   (populate list (send field get-value)))]))

(define (set-data stop-list stops)
  (send/apply stop-list set (let-values ([(names)
                                          (for/lists (names)
                                            ([stop stops])
                                            (values (~a (stop-name stop))))])
                              (list names))))

(define (populate stop-list [filter-expr "*"])
  (begin
    (set-data stop-list (stops))
    (send stop-list set-column-width 0 200 200 400)))

(define stops1 (create-stop-list main-frame))
(populate stops1)
(create-filter-field main-frame stops1)


(send main-frame show #t)