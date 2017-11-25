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
    
    (define selection-panel (new horizontal-panel%
                                 [parent panel]))

    (define selector1 (new compound-stop-selector%
                           [initial-stops stops]
                           [parent selection-panel]
                           [selection-id 'stop1]
                           [callback (lambda (id new-stop)
                                       (display-routes))]
                           [focus #t]))

    (define selector2 (new compound-stop-selector%
                           [initial-stops stops]
                           [parent selection-panel]
                           [selection-id 'stop2]
                           [callback (lambda (id new-stop)
                                       (display-routes))]
                           [focus #f]))

    (define route-display (new route-display%
                               [parent panel]))

    (define (display-routes)
      (let ([compound-stop1 (send selector1 get-selected-stop)]
            [compound-stop2 (send selector2 get-selected-stop)])
        (when (and compound-stop1 compound-stop2)
          (let* ([stops1 (constituents compound-stop1)]
                 [stops2 (constituents compound-stop2)]
                 [routes (~> (for*/list ([stop1 stops1]
                                         [stop2 stops2])
                               (let* ([routes1 (send provider routes-for-stop (stop-id stop1))]
                                      [routes2 (send provider routes-for-stop (stop-id stop2))]
                                      [common-routes (set-intersect routes1 routes2)])
                                 common-routes))
                             flatten
                             list->set)])
            (send route-display show-routes routes)))))

    ))

(provide info-panel%)