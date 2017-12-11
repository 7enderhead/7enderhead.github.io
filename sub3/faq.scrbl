#lang scribble/manual

@(require (for-label "data-defs.rkt" "data-provider.rkt" "db-data-provider.rkt"))

@title[#:tag "faq"]{Frequently Asked Questions}

@section{High Level}

@subsection{What is the basic architecture?}

@itemlist[
 @item{independent database}
 @item{data retrieval via @italic{data access layer} in interface @racket[data-provider<%>], implemented by class @racket[db-data-provider%]}
 @item{@italic{domain entities} (e.g., @racket[route], @racket[stop]) modeled as Racket structures  (see @secref["data-defs"])}
 @item{GUI implemented via the @italic{Racket Graphical Interface Toolkit} (@url["https://docs.racket-lang.org/gui"])}
 @item{in general, see @secref["architecture"]}
 ]

@subsection{How is data handled?}

@subsubsection{In @racket[db-data-provider%]?}

@itemlist[
 @item{data caching in top-level variables}
 @item{routes data can change -> timer-triggered check of table's checksum; cache update and client callback on change}
 ]

@subsubsection{In the GUI?}

@itemlist[
 @item{@racket[stop] data fully retrieved and cached (no updates; small data volume) -> good performance}
 @item{@racket[route] data (display between stops or checks before creation) queried via @racket[data-provider<%>]}
 @item{@racket[route] @italic{creation}: data consistency checked as much as possible on gui (button only active if ok); creation via @racket[data-provider<%>]}
 ]

@section{GUI}

@subsection{Basic GUI Concept?}

@itemlist[
 @item{tabbing for the two big different tasks: display and route creation; see @seclink["info-panel" "info panel"] and @seclink["edit-panel" "edit panel"]}
 @item{@italic{bound input} whenever possible (sliders for longitude/latitude)}
 @item{hints and deactivated 'Create New Route' button instead of modal 'you did wrong' dialog}
 ]

@subsection{Structuring on implementation level?}

@itemlist[
 @item{more complicated compound controls glued together from basic Racket controls (e.g., @seclink["info-panel" "info panel"], @seclink["edit-panel" "edit panel"], @seclink["compound-stop-selector" "compound stop selector"])}
 ]

@section{Racket}

@subsection{What is Racket?}

@itemlist[
 @item{a dialect of the @hyperlink["https://en.wikipedia.org/wiki/Scheme_(programming_language)" "Scheme programming language"], which itself is one of the major dialects of the @hyperlink["https://en.wikipedia.org/wiki/Lisp_(programming_language)" "Lisp programming language"]}
 ]

@subsection{What is special about Racket?}

@itemlist[
 @item{dynamically typed}
 @item{fast feedback cycle via REPL (read-evaluate-print loop), i.e. explorative programming without long build/compile cycles}
 @item{extensive library, e.g. for GUI and Web programming, which is very well documented}
 @item{multi-platform (Win/Linux/macOS)}
 ]