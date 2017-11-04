#lang racket

(require racket/gui/base)
(require threading)
(require (prefix-in util: "util.rkt"))

;;; initialization

(define max-position-integer 1000000)
(define min-position-integer (- max-position-integer))

(define test-slider
  (new slider% [label ""]
       [parent (new frame% [label ""])]
       [min-value min-position-integer]
       [max-value max-position-integer]))

; integer range for slider controls,
; tested from a real slider
(define slider-min (begin
                     (send test-slider set-value min-position-integer)
                     (send test-slider get-value)))

(define slider-max (begin
                     (send test-slider set-value max-position-integer)
                     (send test-slider get-value)))

(set! test-slider #f)


;;; converter class

(define slider-converter%
  (class object%

(super-new)
    
    (init min-lon max-lon min-lat max-lat)

    (define _min-lon min-lon)
    (define _max-lon max-lon)
    (define _min-lat min-lat)
    (define _max-lat max-lat)
    
    (define (slider->value slider-value min-value max-value)
      (util:map-range slider-value
                      slider-min
                      slider-max
                      min-value
                      max-value))

    (define (value->label prefix value)
      (format "~a: ~a" prefix (~> value
                                  util:coord->string)))

    (define (lon-slider-value->label prefix value)
      (value->label prefix (slider->lon value)))

    (define (lat-slider-value->label prefix value)
      (value->label prefix (slider->lat value)))
    
    (define/public (slider->lon value)
      (slider->value value _min-lon _max-lon))

    (define/public (slider->lat value)
      (slider->value value _min-lat _max-lat))

    (define/public (min-lon-label value)
      (lon-slider-value->label "min. Lon." value))
  
    (define/public (max-lon-label value)
      (lon-slider-value->label "max. Lon." value))
  
    (define/public (min-lat-label value)
      (lat-slider-value->label "min. Lat." value))
  
    (define/public (max-lat-label value)
      (lat-slider-value->label "max. Lat." value))
    
    ))

(provide slider-min
         slider-max
         slider-converter%)