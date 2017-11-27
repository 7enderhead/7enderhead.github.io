#lang racket

(require racket/gui/base)
(require sugar/coerce)
(require anaphoric)
(require setup/getinfo)
(require "data-defs.rkt")
(require "util.rkt")
(require "compound-stop-selector.rkt")
(require "data-list-box.rkt")
(require "info-message.rkt")

(define info (get-info/full "."))
(define route-min-stops (info 'route-min-stops))

(define horiz-margin 10)

(define edit-panel%
  (class object%

    (init parent provider)

    (super-new)

    (define panel (new vertical-panel%
                       [parent parent]
                       [alignment '(left top)]))

    (define data-panel
      (new group-box-panel%
           [parent panel]
           [label "Data for New Route"]
           [font large-font]
           [alignment '(left top)]
           [horiz-margin horiz-margin]
           [border 10]
           [spacing 10]
           [stretchable-height #f]))

    (define number-type-panel
      (new horizontal-panel%
           [parent data-panel]
           [alignment '(left center)]
           [spacing 20]))

    (define number-field
      (new text-field%
           [parent number-type-panel]
           [label "Number: "]
           [stretchable-width #f]
           [min-width 100]
           [callback (lambda (control event) (data-ok?))]))
    
    (define type-choice
      (new radio-box%
           [label "Type: "]
           [parent number-type-panel]
           [choices '("Bus" "Tram")]
           [callback (lambda (control event) (data-ok?))]))

    (define start-end-panel
      (new horizontal-panel%
           [parent data-panel]
           [spacing 20]))
    
    (define start-field
      (new text-field%
           [parent start-end-panel]
           [label "Start: "]
           [callback (lambda (control event) (data-ok?))]))

    (define end-field
      (new text-field%
           [parent start-end-panel]
           [label "End: "]
           [callback (lambda (control event) (data-ok?))]))

    (define data-message
      (new info-message%
           [parent data-panel]
           [label "Please enter data for Number, Start and End."]))
    
    (define exists-message
      (new info-message%
           [parent data-panel]
           [label "Route with this data already exists."]))

    (define stop-panel
      (new group-box-panel%
           [parent panel]
           [label "Stops for New Route"]
           [font large-font]
           [alignment '(left top)]
           [horiz-margin horiz-margin]
           [vert-margin 30]
           [border 10]
           [spacing 10]))

    (define inner-stop-panel
      (new horizontal-panel% [parent stop-panel]))
    
    (define stop-selector (new compound-stop-selector%
                               [parent inner-stop-panel]
                               [initial-stops (send provider stops)]
                               [allow-compounds #f]))

    (define button-panel (new vertical-panel%
                              [parent inner-stop-panel]
                              [alignment '(center center)]
                              [stretchable-width #f]
                              [horiz-margin 10]))
    
    (define add-button
      (new button%
           [label "Add Stop ->"]
           [parent button-panel]
           [stretchable-width #t]
           [callback
            (lambda (button event)
              (let ([selected-stop (send stop-selector get-selected-stop)])
                (when selected-stop
                  (send stop-list add-stop selected-stop))))]))

    (define remove-button
      (new button%
           [label "<- Remove Stop"]
           [parent button-panel]
           [stretchable-width #t]
           [callback
            (lambda (button event)
              (send stop-list remove-selected-stop))]))

    (define stop-list-panel (new vertical-panel%
                                 [parent inner-stop-panel]
                                 [spacing 10]))

    (define selection-message
      (new message%
           [label "Stops for New Route"]
           [parent stop-list-panel]
           [font large-font]
           [stretchable-width #t]))

    (define dummy-spacer-message
      (new message%
           [label ""]
           [parent stop-list-panel]
           [font larger-font]
           [stretchable-width #t]))
    
    (define stop-list
      (new
       (class data-list-box%

         (init [data-change-callback #f])
         
         (super-new [label ""]
                    [parent stop-list-panel]
                    [choices '()]
                    [columns '("Name" "Longitude" "Latitude")]
                    [style '(single column-headers)]
                    [meta-data (mutable-set)])

         (send this set-column-widths '(200 200 400) 100 100)
         
         (define (populate)
           (let* ([stops (->list (send this get-meta-data))]
                  [sorted-stops (sort stops
                                      (lambda (stop1 stop2)
                                        (string<? (stop-name stop1) (stop-name stop2))))])
             (send/apply this set (stop-value-lists sorted-stops))
             (for ([index (in-naturals 0)]
                   [stop sorted-stops])
               (send this set-data index stop)))
           (report-data-change))

         (define (selected-stop)
           (if-let [selected-index (send this get-selection)]
                   (send this get-data selected-index)
                   #f))

         (define (report-data-change)
           (when data-change-callback
             (data-change-callback)))
         
         (define/public (add-stop stop)
           (let ([meta-data (send this get-meta-data)])
             (unless (set-member? meta-data stop)
               (set-add! meta-data stop)
               (populate))))

         (define/public (remove-selected-stop)
           (when-let [selected-stop (selected-stop)]
                     (set-remove! (send this get-meta-data) selected-stop)
                     (populate)))

         (define/public (get-all-ids)
           (set-map (send this get-meta-data)
                    (lambda (stop) (stop-id stop))))
         )
       [data-change-callback (lambda () (data-ok?))]
       ))

    (define stop-number-message
      (new info-message%
           [parent stop-panel]
           [label (format "Please select at least ~a stops."
                          route-min-stops)]))

    (define (route-data-ok?)
      (and (non-empty-string? (send number-field get-value))
           (non-empty-string? (send start-field get-value))
           (non-empty-string? (send end-field get-value))))
    
    (define (route-exists?)
      (let* ([new-route (route-from-controls)]
             [exists? (send provider route-exists? new-route)])
        exists?))
    
    (define (stop-number-ok?)
      (>=
       (set-count (send stop-list get-meta-data))
       route-min-stops))

    (define (data-ok?)
      (let* ([data-ok? (route-data-ok?)]
             [exists? (route-exists?)]
             [stop-number-ok? (stop-number-ok?)]
             [all-ok? (and data-ok? (not exists?) stop-number-ok?)])
        (send data-message show (not data-ok?))
        (send exists-message show exists?)
        (send stop-number-message show (not stop-number-ok?))
        (send new-route-button enable all-ok?)
        all-ok?))

    (define (route-from-controls)
      (let ([number (->string (send number-field get-value))]
            [type (->string (send type-choice get-item-label (send type-choice get-selection)))]
            [start (->string (send start-field get-value))]
            [end (->string (send end-field get-value))])
        (route 0 number type start end)))
    
    (define new-route-button
      (new button%
           [parent panel]
           [label "Create New Route"]
           [font large-font]
           [horiz-margin horiz-margin]
           [vert-margin 20]
           [stretchable-width #t]
           [callback
            (lambda (button event)
              (when (data-ok?)
                (send provider
                      insert-route
                      (route-from-controls)
                      (send stop-list get-all-ids))
                (data-ok?) ; no, it's not because of the insert
                ))]))

    (data-ok?) ; initial call
    
    ))

(provide edit-panel%)