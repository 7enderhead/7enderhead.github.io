#lang racket

(require racket/struct)
(require racket/generic)
(require racket/format)
(require threading)
(require "util.rkt")
(require (prefix-in hav: "haversine.rkt"))

(define-generics compoundable
  (constituents compoundable)
  (name compoundable)
  (lon-range compoundable)
  (lat-range compoundable))

;;; Stop

(struct stop (id lon lat name alt-name)
  #:transparent
  #:methods gen:compoundable
  [(define (constituents stop) (set stop))
   (define (name stop) (stop-name stop))
   (define (lon-range stop) (cons (stop-lon stop) (stop-lon stop)))
   (define (lat-range stop) (cons (stop-lat stop) (stop-lat stop)))]
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

;;; Compound Stop

(struct compound-stop (stops)
  #:transparent

  #:methods gen:compoundable
  [(define (constituents compound) (compound-stop-stops compound))

   (define (name compound) (stop-name (set-first (constituents compound))))

   (define (lon-range compound)
     (let ([stops (set->list (constituents compound))])
       (cons (min-lon stops) (max-lon stops))))

   (define (lat-range compound)
     (let ([stops (set->list (constituents compound))])
       (cons (min-lat stops) (max-lat stops))))]

  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (c) 'compound-stop)
      (lambda (c) (list
                   "name" (name c)
                   "lon-range" (format-range (lon-range c))
                   "lat-range" (format-range (lat-range c))
                   (compound-stop-stops c)))))])

(define (format-range range)
  (match-let ([(cons min max) range])
    (if (equal? min max)
        (coord->string min)
        (format "~a - ~a"
                (coord->string min)
                (coord->string max)))))

(define (range-< range1 range2)
  (match-let ([(cons min1 _) range1]
              [(cons min2 _) range2])
    (< min1 min2)))

(define (wrap-stops-as-compounds stops)
  (map (lambda (stop) (compound-stop (set stop))) stops))

(define (compound-stops-by-name stops)
  (~>> stops
       (group-by (lambda (stop) (stop-name stop)))
       (map (lambda (stops) (compound-stop (list->set stops))))))

(define (all-constituents compounds)
  (set->list (for/fold
              ([all (mutable-set)])
              ([compound compounds])
               (set-union! all (constituents compound))
               all)))

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