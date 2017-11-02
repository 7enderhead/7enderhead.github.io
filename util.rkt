#lang racket

(require setup/getinfo)

(define info (get-info/full "."))

(define (coord->string c)
  (~a (exact->inexact c) #:width (info 'coord-string-length) #:right-pad-string "0"))

(define (map-range x in-min in-max out-min out-max)
  (let ([factor (/ (- out-max out-min) (- in-max in-min))])
    (+ (* (- x in-min) factor) out-min)))

(define (filter-expr-match? expr string)
  (with-handlers ([(lambda (e) #t) (lambda (e) #t)])
    (regexp-match? expr string)))

(provide (all-defined-out))