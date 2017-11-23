#lang racket

(require sugar/coerce)
(require "data-provider.rkt")
(require "data-defs.rkt")

(define test-data-provider%
  (class* object% (data-provider<%>)

    (init)

    (super-new)

    (define all-stops
      (for/list ([index (range 1 10)])
        (stop index
              (* 10 index)
              (+ index (* 10 index))
              (format "Stop_~a" index)
              (format "Alt_Stop_~a" index))))

    (define all-routes
      (for/list ([index (range 1 10)])
        (route index
               "Bus"
               (->string index)
               (format "Start_~a" index)
               (format "End_~a" index)
               )))
    
    (define/public (stops)
      all-stops)

    (define/public (stops-by-id)
      (group-stops-by-id all-stops))

    (define/public (routes)
      all-routes)
    
    (define/public (routes-for-stop stop-id)
      '())
      
    ))

(provide test-data-provider%)