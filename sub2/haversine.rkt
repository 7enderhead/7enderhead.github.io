#lang racket

#|
Taken from https://github.com/acmeism/RosettaCodeData/blob/master/Task/Haversine-formula/Racket/haversine-formula.rkt
which itself extract the code from rosettacode.org, where it is published under the GNU Free Documentation License 1.2.
|#

(require math)

(define earth-radius 6371)

(define (distance lat1 long1 lat2 long2)
  (define (h a b) (sqr (sin (/ (- b a) 2))))
  (* 2 earth-radius
     (asin (sqrt (+ (h lat1 lat2)
                    (* (cos lat1) (cos lat2) (h long1 long2)))))))

(define (deg-to-rad d [m 0] [s 0])
  (* (/ pi 180) (+ d (/ m 60) (/ s 3600))))

(provide (all-defined-out))