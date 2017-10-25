#lang racket

(require racket/gui/base)
(require racket/format)
(require threading)
(require anaphoric)
(require "structs.rkt")
(require (prefix-in db: "db.rkt"))

;;; data

(define cached-stops (db:stops))

(define (stops) cached-stops)

(define (process-filter stops processor accessor)
  (apply processor (map (lambda (stop) (accessor stop)) stops)))

(define (min-lon stops) (process-filter stops min stop-lon))
(define (max-lon stops) (process-filter stops max stop-lon))
(define (min-lat stops) (process-filter stops min stop-lat))
(define (max-lat stops) (process-filter stops max stop-lat))

;;; gui conversions

; integer range for slider controls
(define slider-max 16960)
(define slider-min (- slider-max))

(define (slider->value value min-value max-value)
  (map-range value slider-min slider-max min-value max-value))

(define slider->lon (curryr slider->value (min-lon (stops)) (max-lon (stops))))
(define slider->lat (curryr slider->value (min-lat (stops)) (max-lat (stops))))

(define (slider-value->label converter prefix value)
  (format "~a: ~a" prefix (~> value
                              (converter)
                              (exact->padded))))

(define lon-slider-value->label (curry slider-value->label slider->lon))
(define lat-slider-value->label (curry slider-value->label slider->lat))

(define make-min-lon-label (curry lon-slider-value->label "min. Lon."))
(define make-max-lon-label (curry lon-slider-value->label "max. Lon."))
(define make-min-lat-label (curry lat-slider-value->label "min. Lat."))
(define make-max-lat-label (curry lat-slider-value->label "max. Lat."))

(define (map-range x in-min in-max out-min out-max)
  (let ([factor (/ (- out-max out-min) (- in-max in-min))])
    (+ (* (- x in-min) factor) out-min)))

(define column-mappings #(("Name" stop-name)
                          ("Longitude" stop-lon)
                          ("Latitude" stop-lat)))

(define column-names
  (map (match-lambda [(list name accessor) name])
       (vector->list column-mappings)))

(define (create-stop-list parent)
  (letrec ([filter-name? (lambda () (send filter-checkbox get-value))]
           [filter-lon? (lambda () (send lon-checkbox get-value))]
           [filter-lat? (lambda () (send lat-checkbox get-value))]

           [populate (lambda ([sorting 0])
                       (populate-list list
                                      #:filter-expr (if (send filter-checkbox get-value)
                                                        (send filter-textfield get-value)
                                                        "")
                                      #:min-lon (if (filter-lon?)
                                                    (slider->lon (send min-lon-slider get-value))
                                                    (min-lon (stops)))
                                      #:max-lon (if (filter-lon?)
                                                    (slider->lon (send max-lon-slider get-value))
                                                    (max-lon (stops)))
                                      #:min-lat (if (filter-lat?)
                                                    (slider->lat (send min-lat-slider get-value))
                                                    (min-lat (stops)))
                                      #:max-lat (if (filter-lat?)
                                                    (slider->lat (send max-lat-slider get-value))
                                                    (max-lat (stops)))
                                      #:sorting sorting))]

           [panel (new vertical-panel%
                       [parent parent])]
           
           [list (new list-box%
                      [label ""]
                      [parent panel]
                      [choices '()]
                      [columns column-names]
                      [style '(single column-headers)]
                      [callback (lambda (list event) (when (equal? 'list-box-column (send event get-event-type))
                                                       (populate (send event get-column))))])]

           [filter-panel (new horizontal-panel%
                              [parent panel]
                              [stretchable-height #f])]

           [filter-checkbox (new check-box%
                                 [label "Name filter"]
                                 [parent filter-panel]
                                 [value #t]
                                 [callback (lambda (checkbox event) (populate))])]

           [filter-textfield (new text-field%
                                  [label ""]
                                  [parent filter-panel]
                                  [callback (lambda (filter-textfield event)
                                              (when (filter-name?) (populate)))])]

           [lon-panel (new vertical-panel%
                           [parent panel]
                           [stretchable-height #f])]

           [lon-checkbox (new check-box%
                              [label "Longitude filter"]
                              [parent lon-panel]
                              [callback (lambda (checkbox event) (populate))])]
           
           [min-lon-slider (new slider%
                                [label (make-min-lon-label slider-min)]
                                [parent lon-panel]
                                [min-value slider-min]
                                [max-value slider-max]
                                [init-value slider-min]
                                [style '(plain horizontal)]
                                [callback (lambda (slider event)
                                            (send slider set-label
                                                  (make-min-lon-label (send slider get-value)))
                                            (when (filter-lon?) (populate)))])]

           [max-lon-slider (new slider%
                                [label (make-max-lon-label slider-max)]
                                [parent lon-panel]
                                [min-value slider-min]
                                [max-value slider-max]
                                [init-value slider-max]
                                [style '(plain horizontal)]
                                [callback (lambda (slider event)
                                            (send slider set-label (make-max-lon-label (send slider get-value)))
                                            (when (filter-lon?) (populate)))])]

            [lat-panel (new vertical-panel%
                           [parent panel]
                           [stretchable-height #f])]

           [lat-checkbox (new check-box%
                              [label "Latitude filter"]
                              [parent lat-panel]
                              [callback (lambda (checkbox event) (populate))])]

           [min-lat-slider (new slider%
                                [label (make-min-lat-label slider-min)]
                                [parent lat-panel]
                                [min-value slider-min]
                                [max-value slider-max]
                                [init-value slider-min]
                                [style '(plain horizontal)]
                                [callback (lambda (slider event)
                                            (send slider set-label (make-min-lat-label (send slider get-value)))
                                            (when (filter-lat?) (populate)))])]

           [max-lat-slider (new slider%
                                [label (make-max-lat-label slider-max)]
                                [parent lat-panel]
                                [min-value slider-min]
                                [max-value slider-max]
                                [init-value slider-max]
                                [style '(plain horizontal)]
                                [callback (lambda (slider event)
                                            (send slider set-label (make-max-lat-label (send slider get-value)))
                                            (when (filter-lat?) (populate)))])]
           )
    (send list set-column-width 0 200 200 400)
    (send list set-column-width 1 100 100 100)
    (send list set-column-width 2 100 100 100)
    (populate-list list)
    list))

(define (set-data stop-list stops)
  (send/apply stop-list set (let-values ([(names lons lats)
                                          (for/lists (names lons lats)
                                            ([stop stops])
                                            (values (~a (stop-name stop))
                                                    (~a (exact->padded (stop-lon stop)))
                                                    (~a (exact->padded (stop-lat stop)))))])
                              (list names lons lats)))
  ; associate id as data
  (for ([index (in-naturals 0)]
        [stop stops])
    (send stop-list set-data index (stop-id stop))))

(define (exact->padded e)
  (~a (exact->inexact e) #:width 10 #:right-pad-string "0"))

(define (populate-list stop-list
                       #:filter-expr [filter-expr ""]
                       #:min-lon [min-lon (min-lon (stops))]
                       #:max-lon [max-lon (max-lon (stops))]
                       #:min-lat [min-lat (min-lat (stops))]
                       #:max-lat [max-lat (max-lat (stops))]
                       #:sorting [sorting 0])
  (let ([selected-id (get-selected-id stop-list)])
    (set-data stop-list (~> (filter-stops (stops)
                                          filter-expr
                                          min-lon max-lon
                                          min-lat max-lat)
                            (sort-stops sorting)))
    (set-selection stop-list selected-id)))

(define (filter-stops stops filter-expr min-lon max-lon min-lat max-lat)
  (filter
   (lambda (stop)
     (and (regexp-match
           ; case-insensitive substring match
           (format "(?i:~a)" filter-expr)
           (stop-name stop))
          (>= (stop-lon stop) min-lon)
          (<= (stop-lon stop) max-lon)
          (>= (stop-lat stop) min-lat)
          (<= (stop-lat stop) max-lat)))
   stops))

(define (sort-stops stops sorting-index)
  (let ([accessor (eval (second (vector-ref column-mappings sorting-index))
                        (module->namespace "structs.rkt"))])
    (sort stops (lambda (stop1 stop2)
                  (let ([value1 (accessor stop1)]
                        [value2 (accessor stop2)])
                    (if (string? value1)
                        (string<? value1 value2)
                        (< value1 value2)))))))

(define (get-selected-id stop-list)
  (if-let [selected-index (send stop-list get-selection)]
          (send stop-list get-data selected-index)
          #f))

(define (set-selection stop-list id)
  (when-let [index (index-for-id stop-list id)]
            (send stop-list set-selection index)))

(define (index-for-id stop-list id)
  (for/or ([index (in-range 0 (send stop-list get-number))])
    (let ([current-id (send stop-list get-data index)])
      (if (equal? current-id id) index #f))))

;;; initialisation

(define main-frame (new frame% [label "Stops"]
                   [width 400]
                   [height 800]))
 
(define stops1 (create-stop-list main-frame))
#;(define stops2 (create-stop-list main-frame))

(send main-frame show #t)