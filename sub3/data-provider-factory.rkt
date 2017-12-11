#lang racket

(require setup/getinfo)
(require "db-data-provider.rkt")
(require "test-data-provider.rkt")

(define info (get-info/full "."))

(define provider #f)

(define (data-provider)
  (when (not provider)
    (set! provider (new db-data-provider%
                          [server (info 'db-server)]
                          [user (info 'db-user)]
                          [password (info 'db-password)]
                          [database (info 'db-name)]
                          [poll-millisecs (info 'db-poll-millisecs)]))
    ; toggle for test provider
    #;(set! provider (new test-data-provider%)))
  provider)

(provide data-provider)