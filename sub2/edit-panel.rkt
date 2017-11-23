#lang racket

(require racket/gui/base)
(require "util.rkt")

(define edit-panel%
  (class object%

    (init parent provider)

    (super-new)

    (define data-panel
      (new vertical-panel%
           [parent parent]
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
    
    ))

(provide edit-panel%)