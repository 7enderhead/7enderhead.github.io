#lang racket

(require racket/struct)
(require racket/serialize)

(serializable-struct
 web-state (stops route food)
 #:transparent
 #:methods gen:custom-write
 [(define write-proc
    (make-constructor-style-printer
     (lambda (s) 'web-state)
     (lambda (s) (list "stops" (web-state-stops s)
                       "route" (web-state-route s)
                       "food" (web-state-food s)))))])

(serializable-struct
 stops-state (list1 list2)
 #:transparent
 #:methods gen:custom-write
 [(define write-proc
    (make-constructor-style-printer
     (lambda (s) 'stops-state)
     (lambda (s) (list "state1" (stops-state-list1 s)
                       "state2" (stops-state-list2 s)))))])

(serializable-struct
 stop-list-state (stop
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

(serializable-struct
 route-state (number type start end list stops submit-type messages)
 #:transparent
 #:methods gen:custom-write
 [(define write-proc
    (make-constructor-style-printer
     (lambda (s) 'route-state)
     (lambda (s) (list "number" (route-state-number s)
                       "type" (route-state-type s)
                       "start" (route-state-start s)
                       "end" (route-state-end s)
                       "list" (route-state-list s)
                       "stops" (route-state-stops s)
                       "submit-type" (route-state-submit-type s)
                       "messages" (route-state-messages s)))))])

(provide (all-defined-out))