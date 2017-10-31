#lang racket

(require racket/gui/base)
(require "route.rkt")
(require "data-list-box.rkt")

(define route-display% (class object%
                         (init parent)

                         (super-new)

                         (define current-data '())
                         
                         (define display-list
                           (let ([list (new data-list-box%
                                            [label ""]
                                            [parent parent]
                                            [choices '()]
                                            [style '(single column-headers)]
                                            [columns '("Type" "Number" "Start" "End")])])
                             (send list set-column-widths 50 50 300 300)
                             list))

                         (define/public (show-routes routes)
                           (set! current-data routes)
                           (send/apply display-list set
                                       (let-values ([(types numbers starts ends)
                                                     (for/lists (types numbers starts ends)
                                                       ([route routes])
                                                       (values (route-type route)
                                                               (route-number route)
                                                               (route-start route)
                                                               (route-end route)))])
                                         (list types numbers starts ends))))))

(define main-frame (new frame% [label "Stops"]
                        [width 400]
                        [height 800]))

(define display (new route-display% [parent main-frame]))

(send main-frame show #t)

(provide (all-defined-out))