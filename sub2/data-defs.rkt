#lang racket

(require racket/struct)
(require racket/generic)
(require racket/format)
(require threading)
(require "util.rkt")
(require (prefix-in hav: "haversine.rkt"))

(define-generics compoundable
  (constituents compoundable))

;;; Stop

(struct stop (id lon lat name alt-name)
  #:transparent
  #:methods gen:compoundable
  [(define (constituents stop) (list stop))]
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

(define (process-filter stops processor accessor [default-value #f])
  (if (not (empty? stops))
      (apply processor (map (lambda (stop) (accessor stop)) stops))
      default-value))

(define default-min 0)
(define default-max 0)

(define (min-lon stops) (process-filter stops min stop-lon default-min))
(define (max-lon stops) (process-filter stops max stop-lon default-max))
(define (min-lat stops) (process-filter stops min stop-lat default-min))
(define (max-lat stops) (process-filter stops max stop-lat default-max))

;;; Compound

(struct compound-stop (stops)
  #:transparent
  #:methods gen:compoundable
  [(define (constituents compound) (compound-stop-stops compound))]
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (c) 'compound-stop)
      (lambda (c) (list (compound-stop-stops c)))))])

(define (compound-stops-by-name stops)
  (~>> stops
       (group-by (lambda (stop) (stop-name stop)))
       (map (lambda (stops) (compound-stop stops)))))

;;; Route

(struct route (id type number start end)
  #:transparent
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (r) 'route)
      (lambda (r) (list (route-id r)
                        (route-type r)
                        (route-number r)
                        (route-start r)
                        (route-end r)))))])

(provide (all-defined-out))