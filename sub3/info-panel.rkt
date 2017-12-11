#lang scribble/lp2

@defmodule["info-panel.rkt"]

@title[#:tag "info-panel"]{Display of Routes Between Stops}

@image["./doc/info-tab.png"]

@section{Info Panel}

All work is done by a dedicated @racket[info-panel%] class.

@defclass[info-panel% vertical-panel% ()]{
 @itemlist[
 @item{groups its children by a labelled group box}
 @item{arranges two @racket[compound-stop-selector%]s side-by-side}
 @item{displays the common routes between the selected stops in a @racket[route-display%] at the bottom}
 ]
}

@chunk[<info-panel%>
       (define info-panel%
         (class vertical-panel%

           (init parent provider)

           (super-new [parent parent])

           (define stops (send provider stops))

           (define selection-group-panel (new group-box-panel%
                                              [parent this]
                                              [label "Stop Selection"]
                                              [border 10]))

           <compound-stop-selector-placement>

           <route-display-placement>

           <calculation-of-displayed-routes>

           (send provider add-callback display-routes)))]

@subsection{Compound Stop Selector Placement}

@itemlist[
 @item{horizontal panel with a dummy spacer panel in between}
 @item{(panel margin would also work to the 'outside', which we don't want)}
 ]

@chunk[<compound-stop-selector-placement>
       (define selection-panel (new horizontal-panel%
                                    [parent selection-group-panel]))

       (define selector1 (new compound-stop-selector%
                              [initial-stops stops]
                              [parent selection-panel]
                              [selection-id 'stop1]
                              [callback (lambda (id new-stop)
                                          (display-routes))]
                              [focus #t]))

       (new panel%
            [parent selection-panel]
            [min-width 30]
            [stretchable-width #f])
    
       (define selector2 (new compound-stop-selector%
                              [initial-stops stops]
                              [parent selection-panel]
                              [selection-id 'stop2]
                              [callback (lambda (id new-stop)
                                          (display-routes))]
                              [focus #f]))]

@chunk[<route-display-placement>
       (new panel%
            [parent this]
            [min-height 30]
            [stretchable-height #f])
    
       (define route-display-panel (new group-box-panel%
                                        [parent this]
                                        [label "Routes for Selected Stops"]
                                        [border 10]))
    
       (define route-display (new route-display%
                                  [parent route-display-panel]))]

@subsection{Calculation of Displayed Routes}

@itemlist[
 @item{get selected compound stops}
 @item{get routes for pairwise combination of single constituent stops}
 ]

@chunk[<calculation-of-displayed-routes>
       (define (display-routes)
         (send route-display show-routes null)
         (let ([compound-stop1 (send selector1 get-selected-stop)]
               [compound-stop2 (send selector2 get-selected-stop)])
           (when (and (and compound-stop1 compound-stop2)
                      (not (equal? compound-stop1 compound-stop2)))
             (let* ([stops1 (constituents compound-stop1)]
                    [stops2 (constituents compound-stop2)]
                    [routes (routes-for-all-stop-pairs stops1 stops2)])
               (send route-display show-routes routes)))))

       <routes-for-all-stop-pairs>]

@chunk[<routes-for-all-stop-pairs>
       (define (routes-for-all-stop-pairs stops1 stops2)
         (~> (for*/list ([stop1 stops1]
                         [stop2 stops2])
               (let* ([routes1 (send provider routes-for-stop (stop-id stop1))]
                      [routes2 (send provider routes-for-stop (stop-id stop2))]
                      [common-routes (set-intersect routes1 routes2)])
                 common-routes))
             flatten
             remove-duplicates
             sort-routes))]

@section{Code Structure}

@chunk[<*>
       <requires>

       <info-panel%>

       (provide info-panel%)
       ]

@subsection{Required Imports}

@chunk[<requires>
       (require racket)
       (require racket/gui/base)
       (require threading)
       (require "data-defs.rkt")
       (require "compound-stop-selector.rkt")
       (require "route-display.rkt")]