#lang web-server/insta

(require racket/format)
(require anaphoric)
(require threading)
(require setup/getinfo)
(require sugar/coerce)
(require web-server/http/bindings)
(require web-server/formlets)
(require web-server/formlets/input)
(require web-server/formlets/lib)
(require web-server/servlet/web-cells)
(require (rename-in "data-defs.rkt"
                    (min-lon def:min-lon)
                    (max-lon def:max-lon)
                    (min-lat def:min-lat)
                    (max-lat def:max-lat)))
(require "data-provider.rkt")
(require "data-provider-factory.rkt")
(require "list-layout.rkt")
(require "stop-formlet-state.rkt")
(require "util.rkt")

;; debug imports
(require web-server/http/request-structs)

(static-files-path "htdocs")

(define info (get-info/full "."))

(define provider (data-provider))

(define stops (~> (send provider stops)
                  (sort (lambda (stop1 stop2)
                          (string<? (stop-name stop1)
                                    (stop-name stop2))))))

(define min-lon (def:min-lon stops))
(define max-lon (def:max-lon stops))
(define min-lat (def:min-lat stops))
(define max-lat (def:max-lat stops))

(define default-stop-list-state
  (stop-list-state #f (list-layout "" min-lon max-lon min-lat max-lat 0) #t #f #f))

(define default-stop-formlet-state
  (stop-formlet-state default-stop-list-state
                      default-stop-list-state))

(define formlet-state
  (make-web-cell default-stop-formlet-state))

(define (start request)
  (render-stop-info-page request))

(define (stop-list-input stops current-stop)
  (multiselect-input
   stops
   #:multiple? #f
   #:attributes `((size ,(->string (info 'web-stop-list-size))))
   #:display (lambda (stop)
               (format "~a (~a / ~a)"
                       (stop-name stop)
                       (format-range (lon-range stop))
                       (format-range (lat-range stop))))
   #:selected? (lambda (stop)
                 (equal? stop current-stop))))

(define (checkbox-input checked?)
  (cross (pure (lambda (x) (and x #t)))
         (checkbox "" checked?)))

(define (default-text-input preset-value)
  (to-string (default #"" (text-input
                           #:value preset-value))))

(define (number-input preset min max default-value)
  (~> (input #:type "number"
             #:attributes
             `((value ,(->string preset))
               (min ,(->string min))
               (max ,(->string max))
               (step "0.0000001")))
      (default (string->bytes/utf-8 (->string default-value)) _)
      to-string
      to-number))

(define (filter-defs state)
  (let* ([layout (stop-list-state-layout state)]
         [filter-layout (list-layout (if (stop-list-state-use-name-filter? state)
                                         (list-layout-filter-expr layout)
                                         "")
                                     (if (stop-list-state-use-lon-filter? state)
                                         (list-layout-min-lon layout)
                                         min-lon)
                                     (if (stop-list-state-use-lon-filter? state)
                                         (list-layout-max-lon layout)
                                         max-lon)
                                     (if (stop-list-state-use-lat-filter? state)
                                         (list-layout-min-lat layout)
                                         min-lat)
                                     (if (stop-list-state-use-lat-filter? state)
                                         (list-layout-max-lat layout)
                                         max-lat)
                                     0)])
    #;(printf "filter-defs ~a -> ~a~n" state filter-layout)
    filter-layout))

(define (stop-list-formlet state)
  (let* ([current-stop (stop-list-state-stop state)]
         [layout (stop-list-state-layout state)]
         [stops (filter-stops stops (filter-defs state))])
    (formlet
     (#%#
      (div ([class "column"])
           (h2 ,(if current-stop
                    (stop-name current-stop)
                    "no stop selected"))
           (h3 ,(if current-stop
                    (format "(~a / ~a)"
                            (format-range (lon-range current-stop))
                            (format-range (lat-range current-stop)))
                    "-"))
           (div ,{(stop-list-input stops current-stop) . => . selected-stops})
           (div ,{(checkbox-input (stop-list-state-use-name-filter? state))
                  . => . use-name-filter?}
                "Name filter "
                ,{(default-text-input
                    (list-layout-filter-expr (stop-list-state-layout state)))
                  . => . name-filter})

           (div ,{(checkbox-input (stop-list-state-use-lon-filter? state))
                  . => . use-lon-filter?}
                "Longitude filter")
           (div
            "min. Lon.: "
            ,{(number-input (list-layout-min-lon layout) min-lon max-lon min-lon)
              . => . min-lon})
           (div
            "max. Lon.: "
            ,{(number-input (list-layout-max-lon layout) min-lon max-lon max-lon)
              . => . max-lon})

           (div ,{(checkbox-input (stop-list-state-use-lat-filter? state)) . => . use-lat-filter?}
                "Latitude filter")
           (div
            "min. Lat.: "
            ,{(number-input (list-layout-min-lat layout) min-lat max-lat min-lat)
              . => . min-lat})
           (div
            "max. Lat.: "
            ,{(number-input (list-layout-max-lat layout) min-lat max-lat max-lat)
              . => . max-lat})))
     
     (stop-list-state (if (not (null? selected-stops))
                          (car selected-stops)
                          #f)
                      (list-layout name-filter min-lon max-lon min-lat max-lat 0)
                      use-name-filter? use-lon-filter? use-lat-filter?))))

(define (stop-formlet state)
  (let* ([state1 (stop-formlet-state-list1 state)]
         [state2 (stop-formlet-state-list2 state)])
    (formlet
     (#%#
      (div ([class "row"])
           ,{(stop-list-formlet state1) . => . list-state1}
           ,{(stop-list-formlet state2) . => . list-state2}))
     (stop-formlet-state list-state1 list-state2))))

(define (bindings request)
  (force (request-bindings/raw-promise request)))

(define (has-bindings? request)
  (not (null? (bindings request))))

(define (state-from-request request)
  (if (has-bindings? request)
      (formlet-process (stop-formlet (web-cell-ref formlet-state))
                       request)
      default-stop-formlet-state))

(define stylesheet-link `(link ((rel "stylesheet")
                                (href "/styles.css")
                                (type "text/css"))))

(define (create-stop-info-response state embed/url)
  (response/xexpr
   `(html
     (head (title "route21 - Routes Between Stops")
           ,stylesheet-link)
     (body (h1 "Routes Between Stops")
           (fieldset
            (legend "Stop Selection")
            (form ([action ,(embed/url render-stop-info-page)])
                  ,@(formlet-display (stop-formlet state))
                  (p (input ([type "submit"])))))
           (fieldset
            (legend "Routes for Selected Stops")
            ,(route-table state))))))

(define (route-table state)
  (let* ([stop1 (stop-list-state-stop (stop-formlet-state-list1 state))]
         [stop2 (stop-list-state-stop (stop-formlet-state-list2 state))]
         [routes (routes-for-stops stop1 stop2)]
         [route-entries (route-table-entries routes)])
    `(table ,@route-entries)))

(define (render-stop-info-page request)
  (let* ([new-state (state-from-request request)]
         [response-generator (lambda (embed/url)
                               (create-stop-info-response new-state embed/url))])
    (web-cell-shadow formlet-state new-state)
    (send/suspend/dispatch response-generator)))

(define (routes-for-stops compound-stop1 compound-stop2)
  (if (and (and compound-stop1 compound-stop2)
           (not (equal? compound-stop1 compound-stop2)))
      (let* ([stops1 (constituents compound-stop1)]
             [stops2 (constituents compound-stop2)]
             [routes (routes-for-all-stop-pairs provider stops1 stops2)])
        routes)
      null))

(define (route-table-entries routes)
  (for/list ([route routes])
    `(tr (td ,(route-type route))
         (td ,(route-number route))
         (td ,(route-start route))
         (td ,(route-end route)))))