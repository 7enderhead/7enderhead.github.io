#lang racket

(require racket/gui/base)
(require "util.rkt")

(define info-message%
  (class horizontal-panel%

    (init parent label)

    (super-new [parent parent]
               [stretchable-height #f])

    (define icon
      (new message%
           [parent this]
           [label 'caution]))
    
    (define text
      (new message%
           [parent this]
           [label label]
           [font info-font]))

    ))

(provide info-message%)