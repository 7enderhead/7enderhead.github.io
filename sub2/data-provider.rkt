#lang racket

(require racket/class/iop)

(define-interface data-provider<%>
  ()
  (stops
   stops-by-id
   routes
   routes-for-stop
   route-exists?
   insert-route))

(provide data-provider<%>)