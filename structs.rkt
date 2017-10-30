#lang racket

(require racket/struct)
(require "haversine.rkt")

(struct stop (id lon lat name alt-name)
  #:transparent
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (s) 'stop)
      (lambda (s) (list (stop-id s)
                        (stop-lon s)
                        (stop-lat s)
                        (stop-name s)
                        (stop-alt-name s)))))])

; in meters
(define (stop-distance s1 s2)
  (* (distance
      (deg-to-rad (stop-lat s1))
      (deg-to-rad (stop-lon s1))
      (deg-to-rad (stop-lat s2))
      (deg-to-rad (stop-lon s2)))
     1000))

(provide (all-defined-out))