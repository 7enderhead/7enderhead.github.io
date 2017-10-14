#lang racket

(require racket/gui/base)
(require racket/format) ; for ~a
(require "db_test.rkt")

; Make a frame by instantiating the frame% class
(define frame (new frame% [label "GUI Test"]
                   [width 200]
                   [height 400]))
 
; Make a static text message in the frame
(define msg (new message% [parent frame]
                          [label "No events so far..."]))
 
; Make a button in the frame
(new button% [parent frame]
             [label "Click Me"]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (send msg set-label "Button click"))])

(define person-list (new list-box% [parent frame]
                         [label "Persons"]
                         [choices '()]
                         [columns '("Id" "Name")]
                         [stretchable-width #t]))

(send/apply person-list set (let-values ([(ids names)
                                          (for/lists (ids names)
                                            ([person (all-persons)])
                                            (values (~a (vector-ref person 0))
                                                    (~a (vector-ref person 1))))])
                              (list ids names)))

; Show the frame by calling its show method
(send frame show #t)