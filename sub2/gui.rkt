#lang racket

(require racket/gui/base)
(require racket/format)
(require racket/class/iop)
(require "data-provider.rkt")
(require "data-provider-factory.rkt")
(require "data-defs.rkt")
(require "stop-selector.rkt")
(require "route-display.rkt")

;;; data

(define provider (data-provider))

(define stops (send/i provider data-provider<%> stops))

;;; initialisation

(define main-frame (new frame% [label "Route21"]
                        [width 1000]
                        [height 800]))

(define selection-panel (new horizontal-panel%
                             [parent main-frame]))

(define selector1 (new stop-selector%
                       [stops stops]
                       [parent selection-panel]
                       [selection-id 'stop1]
                       [callback (lambda (id new-stop)
                                   (display-routes))]
                       [focus #t]))

(define selector2 (new stop-selector%
                       [stops stops]
                       [parent selection-panel]
                       [selection-id 'stop2]
                       [callback (lambda (id new-stop)
                                   (display-routes))]
                       [focus #f]))

(define routes (new route-display%
                    [parent main-frame]))

(define (display-routes)
  (let ([stop1 (send selector1 get-selected-stop)]
        [stop2 (send selector2 get-selected-stop)])
    (when (and stop1 stop2)
      (let* ([routes1 (send/i provider data-provider<%> routes-for-stop (stop-id stop1))]
             [routes2 (send/i provider data-provider<%> routes-for-stop (stop-id stop2))]
             [common-routes (set-intersect routes1 routes2)])
        (send routes show-routes common-routes)))))
  
(send main-frame show #t)