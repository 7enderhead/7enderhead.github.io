#lang racket

(require racket/gui/base)
(require racket/format) ; for ~a
(require threading)
(require anaphoric)
(require "structs.rkt")
(require (prefix-in db: "db.rkt"))

(define cached-stops (db:stops))

(define (stops) cached-stops)

(define main-frame (new frame% [label "Stops"]
                   [width 400]
                   [height 800]))
 
(define (create-stop-list parent)
  (let ([list (new list-box%
                   [label "Stop:"]
                   [parent parent]
                   [choices '()]
                   [columns '("Name" "Longitude" "Latitude")]
                   [style '(single column-headers)])])
    (send list set-column-width 0 200 200 400)
    (send list set-column-width 1 100 100 100)
    (send list set-column-width 2 100 100 100)
    list))

(define (create-filter-field parent list)
  (new text-field%
       [label "Filter:"]
       [parent parent]
       [callback (lambda (field event)
                   (populate list (send field get-value)))]))

(define (set-data stop-list stops)
  (send/apply stop-list set (let-values ([(names lons lats)
                                          (for/lists (names lons lats)
                                            ([stop stops])
                                            (values (~a (stop-name stop))
                                                    (~a (exact->padded (stop-lon stop)))
                                                    (~a (exact->padded (stop-lon stop)))))])
                              (list names lons lats)))
  ; associate id as data
  (for ([index (in-naturals 0)]
        [stop stops])
    (send stop-list set-data index (stop-id stop))))

(define (exact->padded e)
  (~a (exact->inexact e) #:width 10 #:right-pad-string "0"))

(define (filter-stops stops filter-expr)
  (filter
   (lambda (stop)
     (regexp-match
      ; case-insensitive substring match
      (format "(?i:~a)" filter-expr)
      (stop-name stop)))
   stops))

(define (populate stop-list [filter-expr ""])
  (let ([selected-id (get-selected-id stop-list)])
    (set-data stop-list (filter-stops (stops) filter-expr))
    (set-selection stop-list selected-id)))

(define (get-selected-id stop-list)
  (if-let [selected-index (send stop-list get-selection)]
          (send stop-list get-data selected-index)
          #f))

(define (set-selection stop-list id)
  (when-let [index (index-for-id stop-list id)]
            (send stop-list set-selection index)))

(define (index-for-id stop-list id)
  (for/or ([index (in-range 0 (send stop-list get-number))])
    (let ([current-id (send stops1 get-data index)])
      (if (equal? current-id id) index #f))))

; initialisation
(define stops1 (create-stop-list main-frame))
(populate stops1)
(create-filter-field main-frame stops1)

; integer range for slider controls
(define slider-min (- 16960))
(define slider-max 16960)

(define (map-range x in-min in-max out-min out-max)
  (let ([factor (/ (- out-max out-min) (- in-max in-min))])
    (+ (* (- x in-min) factor) out-min)))

(define (lon->slider lon)
  (map-range lon
             (min-lon (stops))
             (max-lon (stops))
             slider-min slider-max))

(define (slider->lon value)
  (map-range value
             slider-min slider-max
             (min-lon (stops))
             (max-lon (stops))))

(define (max-lon stops)
  (apply max (map (lambda (stop) (stop-lon stop)) stops)))

(define (min-lon stops)
  (apply min (map (lambda (stop) (stop-lon stop)) stops)))

(define (lon-slider-value->label prefix value)
  (format "~a: ~a" prefix  (~> value
                               (slider->lon)
                               (exact->padded))))

(define make-min-lon-label (curry lon-slider-value->label "min. Lon."))
(define make-max-lon-label (curry lon-slider-value->label "max. Lon."))

(new slider%
     [label (make-min-lon-label slider-min)]
     [parent main-frame]
     [min-value slider-min]
     [max-value slider-max]
     [init-value slider-min]
     [style '(plain horizontal)]
     [callback (lambda (slider event)
                 (send slider set-label (make-min-lon-label (send slider get-value))))])

(new slider%
     [label (make-max-lon-label slider-max)]
     [parent main-frame]
     [min-value slider-min]
     [max-value slider-max]
     [init-value slider-max]
     [style '(plain horizontal)]
     [callback (lambda (slider event)
                 (send slider set-label (make-max-lon-label (send slider get-value))))])


(send main-frame show #t)