#lang scribble/lp2

@defmodule["data-provider.rkt"]

@title[#:tag "data-provider"]{Interface data-provider<%>}

@section{Documentation}

@definterface[data-provider<%> ()]{

 The @racket[data-provider<%>] interface provides access to a data layer.

 @defmethod[(stops) (listof stop?)]{
  Returns all stops. Since these are never changed, caching can happen.
 }

 @defmethod[(stops-by-id) (hash/c number? stop?)]{
  Returns all stops in a hashset with @racket[stop-id] as their key. Caching may happen.
 }

 @defmethod[(routes) (listof route?)]{
  Returns all routes. Since routes may change, use @racket[add-callback] to register for changes.
 }

 @defmethod[(routes-for-stop [stop stop?]) (listof route?)]{
  Returns all routes that use the given stop.
 }

 @defmethod[(route-exists? [route route?]) boolean?]{
  Takes a @racket[route] structure and checks whether it already exists in the data layer.
  @racket[route]'s @racket[route-id] is ignored.
 }

 @defmethod[(insert-route [stop stop?] [stop-ids (listof number?)]) void]{
  Inserts a new route.

  @racket[stop-ids] is optional and defaults to @racket[null]. If given, all stops referenced in @racket[stop-ids] are linked with the new route.
 }

 @defmethod[(add-callback [callback (void . -> . void)]) void]{
  Adds a callback to the data layer, which is invoked if the routes have changed.
 }
 
}
@section{Implementation}

@chunk[<*>
       (require racket/class)
       
       (define data-provider<%>
         (interface ()
           stops
           stops-by-id
           routes
           routes-for-stop
           route-exists?
           insert-route
           add-callback))
       
       (provide data-provider<%>)]