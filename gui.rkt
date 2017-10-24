#lang racket

(require racket/gui/base)
(require racket/format) ; for ~a
(require anaphoric)
(require "structs.rkt")
(require (prefix-in db: "db.rkt"))

(define (stops) (db:stops))

(define main-frame (new frame% [label "Stops"]
                   [width 400]
                   [height 800]))
 
(define (create-stop-list parent)
  (let ([list (new list-box%
                   [label "Stop:"]
                   [parent parent]
                   [choices '()]
                   [columns '("Name")]
                   [style '(single column-headers)])])
    (send list set-column-width 0 200 200 400)
    list))

(define (create-filter-field parent list)
  (new text-field%
       [label "Filter:"]
       [parent parent]
       [callback (lambda (field event)
                   (populate list (send field get-value)))]))

(define (set-data stop-list stops)
  (send/apply stop-list set (let-values ([(names)
                                          (for/lists (names)
                                            ([stop stops])
                                            (values (~a (stop-name stop))))])
                              (list names)))
  ; associate id as data
  (for ([index (in-naturals 0)]
        [stop stops])
    (send stop-list set-data index (stop-id stop))))

(define (filter-stops stops filter-expr)
  (filter
   (lambda (stop)
     (regexp-match
      ; case-insensitive substring match
      (format "(?i:~a)" filter-expr)
      (stop-name stop)))
   stops))

(define (populate stop-list [filter-expr ""])
  (let ([selected-id (get-selected-id stop-list)])
    (set-data stop-list (filter-stops (stops) filter-expr))
    (set-selection stop-list selected-id)))

(define (get-selected-id stop-list)
  (if-let [selected-index (send stop-list get-selection)]
          (send stop-list get-data selected-index)
          #f))

(define (set-selection stop-list id)
  (when-let [index (index-for-id stop-list id)]
            (send stop-list set-selection index)))

(define (index-for-id stop-list id)
  (for/or ([index (in-range 0 (send stop-list get-number))])
    (let ([current-id (send stops1 get-data index)])
      (if (equal? current-id id) index #f))))

; initialisation
(define stops1 (create-stop-list main-frame))
(populate stops1)
(create-filter-field main-frame stops1)


(send main-frame show #t)