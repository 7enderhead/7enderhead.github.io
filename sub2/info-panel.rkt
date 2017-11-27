#lang racket

(require racket/gui/base)
(require threading)
(require "data-defs.rkt")
(require "compound-stop-selector.rkt")
(require "route-display.rkt")

(define info-panel%
  (class object%

    (init parent provider)

    (super-new)

    (define stops (send provider stops))

    (define panel (new vertical-panel%
                       [parent parent]))

    (define selection-group-panel (new group-box-panel%
                                       [parent panel]
                                       [label "Stop Selection"]
                                       [border 10]))

    
    (define selection-panel (new horizontal-panel%
                                 [parent selection-group-panel]))

    (define selector1 (new compound-stop-selector%
                           [initial-stops stops]
                           [parent selection-panel]
                           [selection-id 'stop1]
                           [callback (lambda (id new-stop)
                                       (display-routes))]
                           [focus #t]))

    (new panel%
         [parent selection-panel]
         [min-width 30]
         [stretchable-width #f])
    
    (define selector2 (new compound-stop-selector%
                           [initial-stops stops]
                           [parent selection-panel]
                           [selection-id 'stop2]
                           [callback (lambda (id new-stop)
                                       (display-routes))]
                           [focus #f]))

    (new panel%
         [parent panel]
         [min-height 30]
         [stretchable-height #f])
    
    (define route-display-panel (new group-box-panel%
                                     [parent panel]
                                     [label "Routes for Selected Stops"]
                                     [border 10]))
    
    (define route-display (new route-display%
                               [parent route-display-panel]))

    (define (display-routes)
      (send route-display show-routes null)
      (let ([compound-stop1 (send selector1 get-selected-stop)]
            [compound-stop2 (send selector2 get-selected-stop)])
        (when (and (and compound-stop1 compound-stop2)
                   (not (equal? compound-stop1 compound-stop2)))
          (let* ([stops1 (constituents compound-stop1)]
                 [stops2 (constituents compound-stop2)]
                 [routes (~> (for*/list ([stop1 stops1]
                                         [stop2 stops2])
                               (let* ([routes1 (send provider routes-for-stop (stop-id stop1))]
                                      [routes2 (send provider routes-for-stop (stop-id stop2))]
                                      [common-routes (set-intersect routes1 routes2)])
                                 common-routes))
                             flatten
                             remove-duplicates
                             (sort (lambda (route1 route2)
                                     (string<? (route-number route1)
                                               (route-number route2)))))])
            (send route-display show-routes routes)))))

    (send provider add-callback display-routes)
    
    ))

(provide info-panel%)