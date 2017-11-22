#lang racket

(require racket/gui/base)
(require framework)
(require threading)
(require "data-provider.rkt")
(require "data-provider-factory.rkt")
(require "data-defs.rkt")
(require "info-panel.rkt")

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
                            (send info-tab show show-info?)
                            (send edit-tab show (not show-info?)))
                          )]))

(define info-tab (new vertical-panel%
                      [parent tab-panel]))

(define info-panel (new info-panel%
                        [parent info-tab]
                        [provider provider]
                        [stops stops]))

(define edit-tab (new vertical-panel%
                        [parent tab-panel]))

(define test-message (new message%
                          [label "Edit"]
                          [parent edit-panel]))

(send edit-tab show #f)

(send main-frame show #t)