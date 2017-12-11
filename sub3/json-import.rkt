#lang racket

(require "data-provider-factory.rkt")

(begin
    (require json)
    (define trams (call-with-input-file "data-import/tram.json" read-json))
    #;(define buses (call-with-input-file "data-import/bus.json" read-json))
    (define stops-by-id (send (data-provider) stops-by-id))
    (for/list ([line trams])
      (let* ([data (hash-ref line 'tags)]
             [number (hash-ref data 'ref)]
             [members (hash-ref line 'members)])
        (for/list ([member members])
          (let ([role (hash-ref member 'role)]
                [id (hash-ref member 'ref)])
            (unless (hash-has-key? stops-by-id id)
              (format "~a: ~a" id role))
            )))))
  