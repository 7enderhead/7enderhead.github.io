#lang racket

(require racket/gui/base)
(require racket/format)
(require racket/struct)
(require threading)
(require anaphoric)
(require "data-defs.rkt")
(require "util.rkt")
(require "data-list-box.rkt")
(require "stop-selector.rkt")
(require (prefix-in db: "db.rkt"))

;;; data

(define cached-stops (db:stops))

(define (stops) cached-stops)

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
                              coord->string)))

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

(struct list-layout (filter-expr min-lon max-lon min-lat max-lat sorting)
  #:transparent
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (l) 'list-layout)
      (lambda (l) (list (list-layout-filter-expr l)
                        (list-layout-min-lon l)
                        (list-layout-max-lon l)
                        (list-layout-min-lat l)
                        (list-layout-max-lat l)
                        (list-layout-sorting l)))))])

(define (create-stop-selection parent [selection-id #f] [callback #f] [focus #f])
  (letrec ([filter-name? (lambda () (send filter-checkbox get-value))]
           [filter-lon? (lambda () (send lon-checkbox get-value))]
           [filter-lat? (lambda () (send lat-checkbox get-value))]

           [list-layout-from-controls
            (lambda ()
              (list-layout
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

           [selected-stop #f]

           [selection-panel (new horizontal-panel%
                                 [parent panel]
                                 [stretchable-width #t]
                                 [stretchable-height #f])]

           [set-selection-message (lambda (stop)
                                    (let ([new-label (if stop
                                                         (format "~a (~a - ~a)"
                                                                 (stop-name stop)
                                                                 (coord->string (stop-lon stop))
                                                                 (coord->string (stop-lat stop)))
                                                         "no stop selected")])
                                      (send selection-message set-label new-label)))]

           [selection-message (new message%
                                   [label ""]
                                   [parent selection-panel]
                                   [font (make-object font%
                                           (+ 2 (send normal-control-font get-size))
                                           (send normal-control-font get-family)
                                           'normal
                                           'bold)]
                                   [stretchable-width #t])]

           [list-sorting 0]
           
           [list (let ([list (new data-list-box%
                                  [label ""]
                                  [parent panel]
                                  [choices '()]
                                  [columns column-names]
                                  [style '(single column-headers)]
                                  [min-height 200]
                                  [callback (lambda (list event)
                                              (let ([event-type (send event get-event-type)])
                                                (cond
                                                  ((equal? event-type 'list-box)
                                                   (let ([new-stop (get-selected-stop list)])
                                                     (when (not (equal? selected-stop new-stop))
                                                       (set! selected-stop new-stop)
                                                       (set-selection-message selected-stop)
                                                       (when callback (callback selection-id new-stop)))))
                                                  ((equal? event-type 'list-box-column)
                                                   (set! list-sorting (send event get-column))))))])])
                   (send list set-column-widths '(300 200 400) 100 100)
                   list)]

           [filter-panel (new horizontal-panel%
                              [parent panel]
                              [stretchable-height #f]
                              [border 10])]

           [filter-checkbox (new check-box%
                                 [label "Name filter"]
                                 [parent filter-panel]
                                 [value #t])]

           [filter-textfield (new text-field%
                                  [label ""]
                                  [parent filter-panel])]

           [lon-panel (new vertical-panel%
                           [parent panel]
                           [stretchable-height #f]
                           [border 10])]

           [lon-checkbox (new check-box%
                              [label "Longitude filter"]
                              [parent (new horizontal-panel% [parent lon-panel])])]
           
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
                           [stretchable-height #f]
                           [border 10])]

           [lat-checkbox (new check-box%
                              [label "Latitude filter"]
                              [parent (new horizontal-panel% [parent lat-panel])])]

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
                                                  (make-max-lat-label (send slider get-value))))])])
    (populate-list list (list-layout-from-controls))
    (new timer%
         [interval 100]
         [notify-callback (lambda ()
                            (populate-list list (list-layout-from-controls)))])
    (when focus (send filter-textfield focus))
    (void)))

(define (set-data stop-list stops)
  (send/apply stop-list set (let-values ([(names lons lats)
                                          (for/lists (names lons lats)
                                            ([stop stops])
                                            (values (~a (stop-name stop))
                                                    (~a (coord->string (stop-lon stop)))
                                                    (~a (coord->string (stop-lat stop)))))])
                              (list names lons lats)))
  ; associate stop structure as data
  (for ([index (in-naturals 0)]
        [stop stops])
    (send stop-list set-data index stop)))

(define (populate-list stop-list new-layout)
  (let ([old-layout (send stop-list get-meta-data)])
    (when (not (equal? old-layout new-layout))
      (send stop-list set-meta-data new-layout)
      (set-data stop-list (~> (filter-stops (stops) new-layout)
                              (sort-stops (list-layout-sorting new-layout)))))))

(define (filter-stops stops list-layout)
  (filter
   (lambda (stop)
     (and (filter-expr-match?
           (format "(?i:~a)" (list-layout-filter-expr list-layout))
           (stop-name stop))
          (>= (stop-lon stop) (list-layout-min-lon list-layout))
          (<= (stop-lon stop) (list-layout-max-lon list-layout))
          (>= (stop-lat stop) (list-layout-min-lat list-layout))
          (<= (stop-lat stop) (list-layout-max-lat list-layout))))
   stops))

(define (sort-stops stops sorting-index)
  (let ([accessor (eval (second (vector-ref column-mappings sorting-index))
                        (module->namespace "data-defs.rkt"))])
    (sort stops (lambda (stop1 stop2)
                  (let ([value1 (accessor stop1)]
                        [value2 (accessor stop2)])
                    (if (string? value1)
                        (string<? value1 value2)
                        (< value1 value2)))))))

(define (get-selected-stop stop-list)
  (if-let [selected-index (send stop-list get-selection)]
          (send stop-list get-data selected-index)
          #f))

;;; initialisation

(define main-frame (new frame% [label "Stops"]
                        [width 400]
                        [height 800]))

(define selection-panel (new horizontal-panel%
                             [parent main-frame]))

(define selector1 (new stop-selector%
                       [stop-getter stops]
                       [parent main-frame]
                       [selection-id 'stop1]
                       [callback (lambda (id new-stop) (printf "~a: ~a\n" id new-stop))]
                       [focus #t]))

#;(create-stop-selection selection-panel 'stop1 (lambda (id new-stop) (printf "~a: ~a\n" id new-stop)) #t)
(create-stop-selection selection-panel 'stop2 (lambda (id new-stop) (printf "~a: ~a\n" id new-stop)))

(send main-frame show #t)