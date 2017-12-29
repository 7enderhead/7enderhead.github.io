#lang racket

(require racket/struct)
(require racket/serialize)

(serializable-struct stop-list-state (stop
                                      layout
                                      use-name-filter?
                                      use-lon-filter?
                                      use-lat-filter?)
                     #:transparent
                     #:methods gen:custom-write
                     [(define write-proc
                        (make-constructor-style-printer
                         (lambda (s) 'stop-list-state)
                         (lambda (s) (list "stop" (stop-list-state-stop s)
                                           "layout" (stop-list-state-layout s)
                                           "use name filter?" (stop-list-state-use-name-filter? s)
                                           "use lon filter?" (stop-list-state-use-lon-filter? s)
                                           "use lat filter?" (stop-list-state-use-lat-filter? s)))))])

(serializable-struct stop-formlet-state (list1 list2)
                     #:transparent
                     #:methods gen:custom-write
                     [(define write-proc
                        (make-constructor-style-printer
                         (lambda (s) 'stop-formlet-state)
                         (lambda (s) (list "state1" (stop-formlet-state-list1 s)
                                           "state2" (stop-formlet-state-list2 s)))))])

(provide (all-defined-out))