#lang racket

(require racket/gui/base)
(require sugar/coerce)
(require "data-defs.rkt")
(require "util.rkt")
(require "compound-stop-selector.rkt")
(require "data-list-box.rkt")

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

    (define number-field (new text-field%
                              [parent data-panel]
                              [label "Number"]))
    
    (define type-choice (new radio-box%
                             [label "Type"]
                             [parent data-panel]
                             [choices '("Bus" "Tram")]))

    (define start-field (new text-field%
                             [parent data-panel]
                             [label "Start"]))

    (define end-field (new text-field%
                           [parent data-panel]
                           [label "End"]))

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
                               [stretchable-width #t]))

    (define stop-list
      (new
       (class data-list-box%

         (super-new [label "Stops for new Route"]
                    [parent stop-panel]
                    [choices '()]
                    [columns '("Name" "Longitude" "Latitude")]
                    [style '(single column-headers)]
                    [meta-data (mutable-set)])

         (define/public (add-stop stop)
           (let ([meta-data (send this get-meta-data)])
             (unless (set-member? meta-data stop)
               (set-add! meta-data stop)
               (populate))))

         (define (populate)
           (let ([meta-data (send this get-meta-data)])
             (send/apply this set (stop-value-lists (->list meta-data)))))
         
         )))
    ))

(provide edit-panel%)