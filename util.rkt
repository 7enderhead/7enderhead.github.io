#lang racket

(require setup/getinfo)

(define info (get-info/full "."))

(define (coord->string c)
  (~a (exact->inexact c) #:width (info 'coord-string-length) #:right-pad-string "0"))

(provide (all-defined-out))