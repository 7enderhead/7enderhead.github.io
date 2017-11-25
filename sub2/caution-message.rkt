#lang racket

(require racket/gui/base)
(require "util.rkt")

(define info-message%
  (new class horizontal-panel%

       (init parent label)

       (super-new parent)

       (define icon
         (new message%
              [parent this]
              [label 'info]))
    
       (define text
         (new message%
              [parent this]
              [label label]
              [font info-font]))

       ))

(provide info-message%)