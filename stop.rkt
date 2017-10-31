#lang racket

(require racket/struct)
(require "util.rkt")
(require (prefix-in hav: "haversine.rkt"))

(struct stop (id lon lat name alt-name)
  #:transparent
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (s) 'stop)
      (lambda (s) (list (stop-id s)
                        (coord->string (stop-lon s))
                        (coord->string (stop-lat s))
                        (stop-name s)))))])

; in meters
(define (distance s1 s2)
  (* (hav:distance
      (hav:deg-to-rad (stop-lat s1))
      (hav:deg-to-rad (stop-lon s1))
      (hav:deg-to-rad (stop-lat s2))
      (hav:deg-to-rad (stop-lon s2)))
     1000))

(define (process-filter stops processor accessor)
  (apply processor (map (lambda (stop) (accessor stop)) stops)))

(define (min-lon stops) (process-filter stops min stop-lon))
(define (max-lon stops) (process-filter stops max stop-lon))
(define (min-lat stops) (process-filter stops min stop-lat))
(define (max-lat stops) (process-filter stops max stop-lat))

(provide (all-defined-out))