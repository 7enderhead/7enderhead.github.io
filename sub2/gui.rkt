#lang scribble/lp2

@title{route21 GUI Application}

An application that displays two tabs for displaying and manipulating route data:

@itemlist[
 @item{@italic{Routes Between Stops}: displays routes which directly connect the selected stops.}
 @item{@italic{New Route Creation}: allows creation of new routes between two or more stops.}
 ]

@image["./doc/gui.png"]

@section{Top-Level Implementation}

Tasks of main application:

@itemlist[
 @item{setup of centrally provided @racket[data-provider<%>] via module @racket[data-provider-factory]}
 @item{setup of gui main window}
 @item{setup of tabbed pane with two tabs and placement of respective controls (@racket[info-panel] and @racket[edit-panel])}
 ]

@subsection{Setup of Data Provider}

@chunk[<provider-setup>
       (define provider (data-provider))]

@subsection{Setup of Main Window}

@chunk[<main-window-setup>
       (define main-frame
         (new (class frame%
                (super-new [label "Route21"]
                           [width 1000]
                           [height 800])
                (define/augment (on-close)
                  (when (exit:user-oks-exit) (exit:exit))))))]

@subsection{Setup of Tabbed Pane}

@itemlist[
 @item{@racket[tab-panel%] only provides the tabs, but not the actual switching}
 @item{so setup of two panels which host the respective controls}
 @item{unneeded panel is removed as panel child (and thus hidden)}
 ]

@chunk[<tab-pane-setup>
       (define tab-panel
         (new tab-panel%
              [parent main-frame]
              [choices '("Routes Between Stops" "New Route Creation")]
              [callback
               (lambda (panel event)
                 (let ([active-tab (if (equal? 0 (send panel get-selection))
                                       info-tab
                                       edit-tab)])
                   (send panel change-children (lambda (children)
                                                 (list active-tab)))))]))

       (define info-tab (new vertical-panel%
                             [parent tab-panel]
                             [border 10]))

       (define info-panel (new info-panel%
                               [parent info-tab]
                               [provider provider]))

       (define edit-tab (new vertical-panel%
                             [parent tab-panel]
                             [border 10]))

       (define edit-panel (new edit-panel%
                               [parent edit-tab]
                               [provider provider]))

       ; hide initially inactive tab
       (send tab-panel delete-child edit-tab)]

@section{File Structure}

@chunk[<*>
       <requires>

       <provider-setup>

       <main-window-setup>

       <tab-pane-setup>
       
       (send main-frame maximize #t)
       (send main-frame show #t)]

@subsection{Required Imports}

@chunk[<requires>
       (require racket)
       (require racket/gui/base)
       (require framework)
       (require threading)
       (require "data-provider.rkt")
       (require "data-provider-factory.rkt")
       (require "data-defs.rkt")
       (require "info-panel.rkt")
       (require "edit-panel.rkt")]