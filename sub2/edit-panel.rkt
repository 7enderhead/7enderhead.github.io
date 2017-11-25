#lang racket

(require racket/gui/base)
(require sugar/coerce)
(require anaphoric)
(require setup/getinfo)
(require "data-defs.rkt")
(require "util.rkt")
(require "compound-stop-selector.rkt")
(require "data-list-box.rkt")

(define info (get-info/full "."))
(define route-min-stops (info 'route-min-stops))

(define edit-panel%
  (class object%

    (init parent provider)

    (super-new)

    (define panel (new vertical-panel%
                       [parent parent]
                       [alignment '(left top)]))
    
    (define data-panel
      (new vertical-panel%
           [parent panel]
           [alignment '(left top)]
           [horiz-margin 10]))

    (define data-label (new message%
                            [parent data-panel]
                            [label "Data for new Route"]
                            [font larger-font]))

    (define number-field
      (new text-field%
           [parent data-panel]
           [label "Number"]
           [callback (lambda (control event) (check-existence))]))
    
    (define type-choice
      (new radio-box%
           [label "Type"]
           [parent data-panel]
           [choices '("Bus" "Tram")]
           [callback (lambda (control event) (check-existence))]))

    (define start-field
      (new text-field%
           [parent data-panel]
           [label "Start"]
           [callback (lambda (control event) (check-existence))]))

    (define end-field
      (new text-field%
           [parent data-panel]
           [label "End"]
           [callback (lambda (control event) (check-existence))]))

    (define exists-message
      (new message%
           [parent data-panel]
           [label "Route with this data already exists"]
           [font info-font]
           [horiz-margin 10]))

    (send exists-message show #f)

    (define (route-exists?)
      (let* ([new-route (route-from-controls)]
             [exists? (send provider route-exists? new-route)])
        exists?))
    
    (define (check-existence)
      (send exists-message show (route-exists?)))

    (define stop-panel (new horizontal-panel%
                            [parent panel]
                            [alignment '(left top)]))

    (define stop-selector (new compound-stop-selector%
                               [parent stop-panel]
                               [initial-stops (send provider stops)]
                               [allow-compounds #f]))

    (define button-panel (new vertical-panel%
                              [parent stop-panel]
                              [alignment '(center center)]))
    
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

    (define remove-button (new button%
                               [label "<- Remove Stop"]
                               [parent button-panel]
                               [stretchable-width #t]
                               [callback
                                (lambda (button event)
                                  (send stop-list remove-selected-stop))]))

    (define stop-list
      (new
       (class data-list-box%

         (init [data-change-callback #f])
         
         (super-new [label ""]
                    [parent stop-panel]
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
       [data-change-callback (lambda () (check-stop-number))]
       ))

    (define stop-number-message
      (new message%
           [parent panel]
           [label (format "Please select at least ~a stops."
                          route-min-stops)]
           [font info-font]
           [horiz-margin 10]))

    (define (stop-number-ok?)
      (let ([ok? (>=
                  (set-count (send stop-list get-meta-data))
                  route-min-stops)])
        (printf "stop-number-ok? (>= ~a ~a) ~a" (set-count (send stop-list get-meta-data)) route-min-stops ok?)
        ok?))

    (define (check-stop-number)
      (println "check-stop-number")
      (send stop-number-message show (not (stop-number-ok?))))

    (define (route-from-controls)
      (let ([number (->string (send number-field get-value))]
            [type (->string (send type-choice get-item-label (send type-choice get-selection)))]
            [start (->string (send start-field get-value))]
            [end (->string (send end-field get-value))])
        (route 0 number type start end)))
    
    (define new-route-button
      (new button%
           [parent panel]
           [label "Create new route"]
           [callback
            (lambda (button event)
              (unless (route-exists?)
                (send provider
                      insert-route
                      (route-from-controls)
                      (send stop-list get-all-ids))))]))

    
    
    
    
    ))

(provide edit-panel%)