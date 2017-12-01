#lang scribble/manual

@(require (for-label "data-defs.rkt" "data-provider.rkt" "db-data-provider.rkt"))

@title[#:tag "architecture"]{Architecture Overview}

@section{Task 2 - Desktop Application}

To recap, here is a typical data retrieval and display cycle as implemented in the desktop application:

@itemlist[
 @item{user performs action on a Racket GUI control}
 @item{interaction of GUI layer with data access layer via interface @racket[data-provider<%>]}
 @item{database-backed implementation @racket[db-data-provider%] interacts with database via queries}
 @item{domain entities (e.g., @racket[route] struct) are created or manipulated}
 @item{domain entity data is processed and displayed on the GUI}
 ]

@image["./doc/Task2-Sequence.svg" #:scale 0.8]

@section{Task 3 - Static Web Application}

@itemlist[
 @item{The @italic{static web application} uses the same database server, data access layer and domain entities as the GUI Application.}
 @item{Web page handling is performed by Racket's @italic{web-server/insta} Domain Specific Language. Running of the web server and handling of callback urls as well as routing to request handler functions are handled by @italic{web-server/insta}.}
 @item{Requests are delivered to handler functions as @italic{s-expression}-formatted Racket data. Responses are described by the @italic{web-server/insta}'s nested @italic{s-expression}s which describe HMTL content. Dynamic content is inserted via Racket's @italic{unquote} mechanisms, e.g., @litchar{,} (@racket[unquote]) to insert one datum or @litchar[",@"] (@racket[unquote-splicing]) to insert a list.}
 ]
Example code handling a request and showing a static web form with dynamic data content embedded in a selection list:
@racketblock[(define (show-all request)
               (define (response-generator embed/url)
                 (response/xexpr
                  `(html
                    (head (title "All"))
                    (body (h1 "List of Persons")
                          (form ([action ,(embed/url show-selected)])
                                (p (select ([size "5"]
                                            [name "person"])
                                           ,@(all-persons)))
                                (p (input ([type "submit"]))))))))
               (send/suspend/dispatch response-generator))]

@image["./doc/Task3-Sequence.svg" #:scale 0.8]

@section{Task 4 - Dynamic Web Application}

@itemlist[
 @item{The @italic{dynamic web application} uses the MySQL database server and data model from Tasks 2 and 3.}
 @item{The JavaScript library @italic{Bootstrap} accesses the database directly via the underlying @italic{jQuery data-table} mechanism.}
 @item{The Racket implementations from Tasks 2 and 3 are not reused due to the following reasons:
  @itemlist[
 @item{a dedicated JavaScript frontend library like @italic{Bootstrap} already has existent methods to perform the requested dynamic content manipulations}
 @item{development of Tasks 3 and 4 can be highly parallelized since no common code base has to be considered}
 ]}
 ]