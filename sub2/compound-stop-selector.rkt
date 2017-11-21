#lang racket

(require racket/gui/base)
(require racket/struct)
(require anaphoric)
(require threading)
(require sugar/coerce)
(require (rename-in "data-defs.rkt"
                    (min-lon def:min-lon)
                    (max-lon def:max-lon)
                    (min-lat def:min-lat)
                    (max-lat def:max-lat)))
(require "data-list-box.rkt")
(require "slider-converter.rkt")
(require "util.rkt")

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

(define column-mappings #(("Name" name)
                          ("Longitude" lon-range)
                          ("Latitude" lat-range)))

(define column-names
  (map (match-lambda [(list name accessor) name])
       (vector->list column-mappings)))

(define compound-stop-selector%
  (class object%

    (init initial-stops parent [selection-id #f] [callback #f] [focus #f])

    (super-new)

    ;;; internal fields
    
    (define selected-stop #f)
    (define list-sorting 0)
    (define stops initial-stops)
    (define compound-stops (compound-stops-by-name initial-stops))
    (define show-compounds? #f)
    (define constituents (->list (all-constituents stops)))

    (define min-lon (def:min-lon constituents))
    (define max-lon (def:max-lon constituents))
    (define min-lat (def:min-lat constituents))
    (define max-lat (def:max-lat constituents))
    
    (define slider-converter
      (new slider-converter%
           [min-lon min-lon]
           [max-lon max-lon]
           [min-lat min-lat]
           [max-lat max-lat]))
      
    ;;; control fields
    
    (define panel (new vertical-panel%
                       [parent parent]))

    (define selection-panel (new horizontal-panel%
                                 [parent panel]
                                 [stretchable-width #t]
                                 [stretchable-height #f]))

    (define selection-message
      (new message%
           [label ""]
           [parent selection-panel]
           [font (make-object font%
                   (+ 2 (send normal-control-font get-size))
                   (send normal-control-font get-family)
                   'normal
                   'bold)]
           [stretchable-width #t]))

    (define compound-panel (new horizontal-panel%
                                [parent panel]
                                [stretchable-height #f]
                                [border 10]))
    
    (define compound-checkbox (new check-box%
                                   [label "Compound stops with same name"]
                                   [parent compound-panel]
                                   [value #f]
                                   [callback
                                    (lambda (checkbox event)
                                      (let ([checked? (send checkbox get-value)])
                                        (when (not (equal? checked? show-compounds?))
                                          (set! show-compounds? checked?)
                                          (if show-compounds?
                                              (set! stops compound-stops)
                                              (set! stops initial-stops))
                                          (populate-list #t))))]))
    
    (define data-list
      (let ([data-list
             (new data-list-box%
                  [label ""]
                  [parent panel]
                  [choices '()]
                  [columns column-names]
                  [style '(single column-headers)]
                  [min-height 200]
                  [callback (lambda (data-list event)
                              (let ([event-type (send event get-event-type)])
                                (cond
                                  ((equal? event-type 'list-box)
                                   (let ([new-stop (selected-stop-from-list)])
                                     (when (not (equal? selected-stop new-stop))
                                       (set! selected-stop new-stop)
                                       (set-selection-message selected-stop)
                                       (when callback (callback selection-id new-stop)))))
                                  ((equal? event-type 'list-box-column)
                                   (set! list-sorting (send event get-column))))))])])
        (send data-list set-column-widths '(300 200 400) 100 100)
        data-list))

    (define filter-panel (new horizontal-panel%
                              [parent panel]
                              [stretchable-height #f]
                              [border 10]))

    (define filter-checkbox (new check-box%
                                 [label "Name filter"]
                                 [parent filter-panel]
                                 [value #t]))

    (define filter-textfield (new text-field%
                                  [label ""]
                                  [parent filter-panel]))

    (define lon-panel (new vertical-panel%
                           [parent panel]
                           [stretchable-height #f]
                           [border 10]))
    
    (define lon-checkbox (new check-box%
                              [label "Longitude filter"]
                              [parent (new horizontal-panel% [parent lon-panel])]))
           
    (define min-lon-slider
      (new slider%
           [label (send slider-converter min-lon-label slider-min)]
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
                             (send slider-converter min-lon-label (send slider get-value))))]))

    (define max-lon-slider
      (new slider%
           [label (send slider-converter max-lon-label slider-max)]
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
                             (send slider-converter max-lon-label (send slider get-value))))]))

    (define lat-panel (new vertical-panel%
                           [parent panel]
                           [stretchable-height #f]
                           [border 10]))

    (define lat-checkbox (new check-box%
                              [label "Latitude filter"]
                              [parent (new horizontal-panel% [parent lat-panel])]))

    (define min-lat-slider
      (new slider%
           [label (send slider-converter min-lat-label slider-min)]
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
                             (send slider-converter min-lat-label (send slider get-value))))]))

    (define max-lat-slider
      (new slider%
           [label (send slider-converter max-lat-label slider-max)]
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
                             (send slider-converter max-lat-label (send slider get-value))))]))
    
    ;;; private methods

    (define (set-selection-message stop)
      (let ([new-label (if stop
                           (format "~a (~a / ~a)"
                                   (name stop)
                                   (format-range (lon-range stop))
                                   (format-range (lat-range stop)))
                           "no stop selected")])
        (send selection-message set-label new-label)))

    (define (filter-name?) (send filter-checkbox get-value))
    (define (filter-lon?) (send lon-checkbox get-value))
    (define (filter-lat?) (send lat-checkbox get-value))
    
    (define (list-layout-from-controls)
      (list-layout
       (if (filter-name?)
           (send filter-textfield get-value)
           "")
       (if (filter-lon?)
           (send slider-converter slider->lon (send min-lon-slider get-value))
           min-lon)
       (if (filter-lon?)
           (send slider-converter slider->lon (send max-lon-slider get-value))
           max-lon)
       (if (filter-lat?)
           (send slider-converter slider->lat (send min-lat-slider get-value))
           min-lat)
       (if (filter-lat?)
           (send slider-converter slider->lat (send max-lat-slider get-value))
           max-lat)
       list-sorting))
    
    (define (populate-list [force? #f])
      (let ([old-layout (send data-list get-meta-data)]
            [new-layout (list-layout-from-controls)])
        (when (or force? (not (equal? old-layout new-layout)))
          (send data-list set-meta-data new-layout)
          (set-data (~> (filter-stops stops new-layout)
                        (sort-stops (list-layout-sorting new-layout))))))) 

    (define (selected-stop-from-list)
      (if-let [selected-index (send data-list get-selection)]
              (send data-list get-data selected-index)
              #f))
    
    (define (set-data stops)
      (send/apply data-list set (let-values ([(names lons lats)
                                              (for/lists (names lons lats)
                                                ([stop stops])
                                                (values (~a (name stop))
                                                        (~a (format-range (lon-range stop)))
                                                        (~a (format-range (lat-range stop)))))])
                                  (list names lons lats)))
      ; associate stop structure as data
      (for ([index (in-naturals 0)]
            [stop stops])
        (send data-list set-data index stop)))

    (define (filter-stops stops list-layout)
      (filter
       (lambda (stop)
         (and (filter-expr-match?
               (format "(?i:~a)" (list-layout-filter-expr list-layout))
               (name stop))
              (>= (car (lon-range stop)) (list-layout-min-lon list-layout))
              (<= (cdr (lon-range stop)) (list-layout-max-lon list-layout))
              (>= (car (lat-range stop)) (list-layout-min-lat list-layout))
              (<= (cdr (lat-range stop)) (list-layout-max-lat list-layout))))
       stops))

    (define (sort-stops stops sorting-index)
      (let ([accessor (eval (second (vector-ref column-mappings sorting-index))
                            (module->namespace "data-defs.rkt"))])
        (sort stops (lambda (stop1 stop2)
                      (let ([value1 (accessor stop1)]
                            [value2 (accessor stop2)])
                        (if (string? value1)
                            (string<? value1 value2)
                            (range-< value1 value2)))))))

    (define update-timer
      (new timer%
           [interval 100]
           [notify-callback (lambda ()
                              (populate-list))]))

    ;;; public methods

    (define/public (get-selected-stop)
      selected-stop)
    ))

(provide compound-stop-selector%)