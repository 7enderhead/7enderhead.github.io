#lang racket

(require racket/gui/base)
(require racket/format)
(require racket/class/iop)
(require "data-defs.rkt")
(require "stop-selector.rkt")
(require "data-provider.rkt")
(require "data-provider-factory.rkt")

;;; data

(define provider (data-provider))

(define stops (send/i provider data-provider<%> stops))

;;; initialisation

(define main-frame (new frame% [label "Stops"]
                        [width 400]
                        [height 800]))

(define selection-panel (new horizontal-panel%
                             [parent main-frame]))

(define selector1 (new stop-selector%
                       [stops stops]
                       [parent selection-panel]
                       [selection-id 'stop1]
                       [callback (lambda (id new-stop) (printf "~a: ~a\n" id new-stop))]
                       [focus #t]))

(define selector2 (new stop-selector%
                       [stops stops]
                       [parent selection-panel]
                       [selection-id 'stop2]
                       [callback (lambda (id new-stop) (printf "~a: ~a\n" id new-stop))]
                       [focus #t]))

(send main-frame show #t)