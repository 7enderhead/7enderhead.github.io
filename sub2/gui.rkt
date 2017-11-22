#lang racket

(require racket/gui/base)
(require framework)
(require racket/format)
(require threading)
(require "data-provider.rkt")
(require "data-provider-factory.rkt")
(require "data-defs.rkt")
(require "compound-stop-selector.rkt")
(require "route-display.rkt")

;;; data

(define provider (data-provider))

(define stops (send provider stops))

;;; initialisation

(define main-frame
  (new (class frame%
         (super-new [label "Route21"]
                    [width 1000]
                    [height 800])
         #;(define/augment (on-close)
           (when (exit:user-oks-exit) (exit:exit))))))

(define tab-panel (new tab-panel%
                       [parent main-frame]
                       [choices '("Info" "Edit")]
                       [callback
                        (lambda (panel event)
                          (let ([show-info? (if (equal? 0 (send panel get-selection)) #t #f)])
                            (send info-panel show show-info?)
                            (send edit-panel show (not show-info?)))
                          )]))

(define info-panel (new vertical-panel%
                        [parent tab-panel]))

(define selection-panel (new horizontal-panel%
                             [parent info-panel]))

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
                           [parent info-panel]))

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
        (send route-display show-routes routes)
        ))))

(define edit-panel (new vertical-panel%
                        [parent tab-panel]))

(define test-message (new message%
                          [label "Edit"]
                          [parent edit-panel]))

(send edit-panel show #f)

(send main-frame show #t)