#lang racket

(require racket/gui/base)
(require racket/format)
(require racket/struct)
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

(define data-list-box% (class list-box%
                           (init label
                                 parent
                                 choices
                                 [columns '("Column")]
                                 [style '(single)]
                                 [callback (lambda (control event) (void))]
                                 [meta-data #f])
                           (super-new [label label]
                                      [parent parent]
                                      [choices choices]
                                      [columns columns]
                                      [style style]
                                      [callback callback])
                           (define the-meta-data meta-data)
                           (define/public (get-meta-data) the-meta-data)
                           (define/public (set-meta-data new-meta-data) (set! the-meta-data new-meta-data))))

(struct list-state (filter-expr min-lon max-lon min-lat max-lat sorting)
  #:transparent
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (s) 'list-state)
      (lambda (s) (list (list-state-filter-expr s)
                        (list-state-min-lon s)
                        (list-state-max-lon s)
                        (list-state-min-lat s)
                        (list-state-max-lat s)
                        (list-state-sorting s)))))])

(define (create-stop-list parent)
  (letrec ([filter-name? (lambda () (send filter-checkbox get-value))]
           [filter-lon? (lambda () (send lon-checkbox get-value))]
           [filter-lat? (lambda () (send lat-checkbox get-value))]

           [list-state-from-controls
            (lambda ()
              (list-state
               (if (filter-name?)
                   (send filter-textfield get-value)
                   "")
               (if (filter-lon?)
                   (slider->lon (send min-lon-slider get-value))
                   (min-lon (stops)))
               (if (filter-lon?)
                   (slider->lon (send max-lon-slider get-value))
                   (max-lon (stops)))
               (if (filter-lat?)
                   (slider->lat (send min-lat-slider get-value))
                   (min-lat (stops)))
               (if (filter-lat?)
                   (slider->lat (send max-lat-slider get-value))
                   (max-lat (stops)))
               list-sorting))]
           
           [panel (new vertical-panel%
                       [parent parent])]

           [list-sorting 0]
           
           [list (new data-list-box%
                      [label ""]
                      [parent panel]
                      [choices '()]
                      [columns column-names]
                      [style '(single column-headers)]
                      [callback (lambda (list event) (when (equal? 'list-box-column (send event get-event-type))
                                                       (set! list-sorting (send event get-column))))])]

           [filter-panel (new horizontal-panel%
                              [parent panel]
                              [stretchable-height #f])]

           [filter-checkbox (new check-box%
                                 [label "Name filter"]
                                 [parent filter-panel]
                                 [value #t])]

           [filter-textfield (new text-field%
                                  [label ""]
                                  [parent filter-panel])]

           [lon-panel (new vertical-panel%
                           [parent panel]
                           [stretchable-height #f])]

           [lon-checkbox (new check-box%
                              [label "Longitude filter"]
                              [parent lon-panel])]
           
           [min-lon-slider (new slider%
                                [label (make-min-lon-label slider-min)]
                                [parent lon-panel]
                                [min-value slider-min]
                                [max-value slider-max]
                                [init-value slider-min]
                                [style '(plain horizontal)]
                                [callback (lambda (slider event)
                                            (let ([max-lon (send max-lon-slider get-value)])
                                              (when (> (send slider get-value) max-lon)
                                                (send slider set-value max-lon)))
                                            (send slider set-label
                                                  (make-min-lon-label (send slider get-value))))])]

           [max-lon-slider (new slider%
                                [label (make-max-lon-label slider-max)]
                                [parent lon-panel]
                                [min-value slider-min]
                                [max-value slider-max]
                                [init-value slider-max]
                                [style '(plain horizontal)]
                                [callback (lambda (slider event)
                                            (let ([min-lon (send min-lon-slider get-value)])
                                              (when (< (send slider get-value) min-lon)
                                                (send slider set-value min-lon)))
                                            (send slider set-label
                                                  (make-max-lon-label (send slider get-value))))])]

            [lat-panel (new vertical-panel%
                           [parent panel]
                           [stretchable-height #f])]

           [lat-checkbox (new check-box%
                              [label "Latitude filter"]
                              [parent lat-panel])]

           [min-lat-slider (new slider%
                                [label (make-min-lat-label slider-min)]
                                [parent lat-panel]
                                [min-value slider-min]
                                [max-value slider-max]
                                [init-value slider-min]
                                [style '(plain horizontal)]
                                [callback (lambda (slider event)
                                            (let ([max-lat (send max-lat-slider get-value)])
                                              (when (> (send slider get-value) max-lat)
                                                (send slider set-value max-lat)))
                                            (send slider set-label
                                                  (make-min-lat-label (send slider get-value))))])]

           [max-lat-slider (new slider%
                                [label (make-max-lat-label slider-max)]
                                [parent lat-panel]
                                [min-value slider-min]
                                [max-value slider-max]
                                [init-value slider-max]
                                [style '(plain horizontal)]
                                [callback (lambda (slider event)
                                            (let ([min-lat (send min-lat-slider get-value)])
                                              (when (< (send slider get-value) min-lat)
                                                (send slider set-value min-lat)))
                                            (send slider set-label
                                                  (make-max-lat-label (send slider get-value))))])]
           )
    (send list set-column-width 0 200 200 400)
    (send list set-column-width 1 100 100 100)
    (send list set-column-width 2 100 100 100)
    (populate-list list (list-state-from-controls))
    (new timer%
         [interval 500]
         [notify-callback (lambda ()
                            (populate-list list (list-state-from-controls)))])
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

(define (populate-list stop-list new-state)
  (let ([selected-id (get-selected-id stop-list)]
        [old-state (send stop-list get-meta-data)])
    (when (not (equal? old-state new-state))
      (displayln "different state")
      (send stop-list set-meta-data new-state)
      (set-data stop-list (~> (filter-stops (stops) new-state)
                              (sort-stops (list-state-sorting new-state))))
      (set-selection stop-list selected-id)))
  ; simulate slow system
  (sleep 0.2))

(define (filter-stops stops list-state)
  (filter
   (lambda (stop)
     (and (regexp-match
           ; case-insensitive substring match
           (format "(?i:~a)" (list-state-filter-expr list-state))
           (stop-name stop))
          (>= (stop-lon stop) (list-state-min-lon list-state))
          (<= (stop-lon stop) (list-state-max-lon list-state))
          (>= (stop-lat stop) (list-state-min-lat list-state))
          (<= (stop-lat stop) (list-state-max-lat list-state))))
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