#lang racket

(require racket/class/iop)

(define-interface data-provider<%>
  ()
  (stops
   routes-for-stop))

(provide data-provider<%>)