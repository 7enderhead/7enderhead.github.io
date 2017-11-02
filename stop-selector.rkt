#lang racket

(require racket/gui/base)
(require racket/struct)
(require anaphoric)
(require threading)
(require "data-defs.rkt")
(require "data-list-box.rkt")
(require "slider.rkt")
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

(define column-mappings #(("Name" stop-name)
                          ("Longitude" stop-lon)
                          ("Latitude" stop-lat)))

(define column-names
  (map (match-lambda [(list name accessor) name])
       (vector->list column-mappings)))

(define stop-selector%
  (class object%
    (init stop-getter parent [selection-id #f] [callback #f] [focus #f])
    (super-new)

    ;;; internal fields
    
    (define selected-stop #f)
    (define list-sorting 0)

      
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

    
    
    (define list
      (let ([list
             (new data-list-box%
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
        #;(populate-list list)
        list))

    
    
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
           [label (make-min-lon-label (stop-getter) slider-min)]
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
                             (make-min-lon-label (stop-getter) (send slider get-value))))]))

    (define max-lon-slider
      (new slider%
           [label (make-max-lon-label (stop-getter) slider-max)]
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
                             (make-max-lon-label (stop-getter) (send slider get-value))))]))

    (define lat-panel (new vertical-panel%
                           [parent panel]
                           [stretchable-height #f]
                           [border 10]))

    (define lat-checkbox (new check-box%
                              [label "Latitude filter"]
                              [parent (new horizontal-panel% [parent lat-panel])]))

    (define min-lat-slider
      (new slider%
           [label (make-min-lat-label (stop-getter) slider-min)]
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
                             (make-min-lat-label (stop-getter) (send slider get-value))))]))

    (define max-lat-slider
      (new slider%
           [label (make-max-lat-label (stop-getter) slider-max)]
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
                             (make-max-lat-label (stop-getter) (send slider get-value))))]))
    
    ;;; private methods

    (define (set-selection-message stop)
      (let ([new-label (if stop
                           (format "~a (~a - ~a)"
                                   (stop-name stop)
                                   (coord->string (stop-lon stop))
                                   (coord->string (stop-lat stop)))
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
           (slider->lon (send min-lon-slider get-value))
           (min-lon (stop-getter)))
       (if (filter-lon?)
           (slider->lon (send max-lon-slider get-value))
           (max-lon (stop-getter)))
       (if (filter-lat?)
           (slider->lat (send min-lat-slider get-value))
           (min-lat (stop-getter)))
       (if (filter-lat?)
           (slider->lat (send max-lat-slider get-value))
           (max-lat (stop-getter)))
       list-sorting))
    
    (define (populate-list)
      (let ([old-layout (send list get-meta-data)]
            [new-layout (list-layout-from-controls)])
        (when (not (equal? old-layout new-layout))
          (send list set-meta-data new-layout)
          (set-data list (~> (filter-stops (stop-getter) new-layout)
                             (sort-stops (list-layout-sorting new-layout))))))) 

    (define (get-selected-stop)
      (if-let [selected-index (send list get-selection)]
              (send list get-data selected-index)
              #f))
    
    (define (set-data stops)
      (send/apply list set (let-values ([(names lons lats)
                                         (for/lists (names lons lats)
                                           ([stop stops])
                                           (values (~a (stop-name stop))
                                                   (~a (coord->string (stop-lon stop)))
                                                   (~a (coord->string (stop-lat stop)))))])
                             (list names lons lats)))
      ; associate stop structure as data
      (for ([index (in-naturals 0)]
            [stop stops])
        (send list set-data index stop)))

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

    (define update-timer
      (new timer%
           [interval 100]
           [notify-callback (lambda ()
                              (populate-list))]))
    ))

(provide stop-selector%)