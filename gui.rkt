#lang racket

(require racket/gui/base)
(require racket/format) ; for ~a
(require "structs.rkt")
(require (prefix-in db: "db.rkt"))

; Make a frame by instantiating the frame% class
(define frame (new frame% [label "Stops"]
                   [width 400]
                   [height 800]))
 
(define stop-list (new list-box% [parent frame]
                         [label "Stops"]
                         [choices '()]
                         [columns '("Id" "Name")]
                         [stretchable-width #t]))

(send/apply stop-list set (let-values ([(ids names)
                                          (for/lists (ids names)
                                            ([stop (db:stops)])
                                            (values (~a (stop-id stop))
                                                    (~a (stop-name stop))))])
                              (list ids names)))

; Show the frame by calling its show method
(send frame show #t)