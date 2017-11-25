#lang racket

(require setup/getinfo)
(require racket/gui/base)

(define info (get-info/full "."))

(define (coord->string c)
  (~a (exact->inexact c) #:width (info 'coord-string-length) #:right-pad-string "0"))

(define (map-range x in-min in-max out-min out-max)
  (let ([factor (/ (- out-max out-min) (- in-max in-min))])
    (+ (* (- x in-min) factor) out-min)))

(define (filter-expr-match? expr string)
  (with-handlers ([(lambda (e) #t) (lambda (e) #t)])
    (regexp-match? expr string)))

(define larger-font (make-object font%
                      (+ 1 (send normal-control-font get-size))
                      (send normal-control-font get-family)
                      'normal
                      'bold))

(define large-font (make-object font%
                     (+ 2 (send normal-control-font get-size))
                     (send normal-control-font get-family)
                     'normal
                     'bold))

(define info-font (make-object font%
                      (+ 1 (send normal-control-font get-size))
                      (send normal-control-font get-family)
                      'italic))

(provide (all-defined-out))