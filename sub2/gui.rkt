#lang racket

(require racket/gui/base)
(require framework)
(require threading)
(require "data-provider.rkt")
(require "data-provider-factory.rkt")
(require "data-defs.rkt")
(require "info-panel.rkt")
(require "edit-panel.rkt")

;;; data

(define provider (data-provider))

(define stops (send provider stops))

;;; initialisation

(define main-frame
  (new (class frame%
         (super-new [label "Route21"]
                    [width 1000]
                    [height 800])
         (define/augment (on-close)
           (when (exit:user-oks-exit) (exit:exit))))))

(define tab-panel
  (new tab-panel%
       [parent main-frame]
       [choices '("Info" "Edit")]
       [callback
        (lambda (panel event)
          (let ([active-tab (if (equal? "Info" (send panel get-item-label (send panel get-selection)))
                                info-tab
                                edit-tab)])
            (send panel change-children (lambda (children)
                                          (list active-tab)))))]))

(define info-tab (new vertical-panel%
                      [parent tab-panel]
                      [border 10]))

(define info-panel (new info-panel%
                        [parent info-tab]
                        [provider provider]))

(define edit-tab (new vertical-panel%
                      [parent tab-panel]
                      [border 10]))

(define edit-panel (new edit-panel%
                      [parent edit-tab]
                      [provider provider]))

(send tab-panel delete-child edit-tab)

(send main-frame show #t)