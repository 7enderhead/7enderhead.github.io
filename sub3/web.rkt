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
(require "web-states.rkt")
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

(define default-stops-state
  (stops-state default-stop-list-state
               default-stop-list-state))

(define default-route-state
  (route-state "" "Bus" "" "" default-stop-list-state null))

(define default-global-state (web-state default-stops-state
                                        default-route-state
                                        null))

(define global-state
  (make-web-cell default-global-state))

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

(define (preset-text-input preset-value [attributes '()])
  (to-string (default #"" (text-input
                           #:value preset-value
                           #:attributes attributes))))

(define (number-input preset min max preset-value)
  (~> (input #:type "number"
             #:attributes
             `((value ,(->string preset))
               (min ,(->string min))
               (max ,(->string max))
               (step "0.0000001")))
      (default (string->bytes/utf-8 (->string preset-value)) _)
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

(define (route-formlet state embed/url)
  (let* ([state state])
    (formlet
     (#%#
      (form ([action ,(embed/url render-route-edit-page)])
            (div ([class "row"])
                 "Number: "
                 ,{(preset-text-input (route-state-number state)
                                      '((required "true"))) . => . number}
                 "Type: "
                 ,{(radio-group '("Bus" "Tram")
                                #:checked? (λ (t) (equal? t (route-state-type state))))
                   . => . type})
            (div ([class "row"])
                 (div ([class "column"])
                      "Start: "
                      ,{(preset-text-input (route-state-start state)
                                           '((required "true"))) . => . start})
                 (div ([class "column"])
                      "End: "
                      ,{(preset-text-input (route-state-end state)
                                           '((required "true"))) . => . end}))
            (div ([class "row"])
                 (div ([class "three-column-outer"])
                      ,{(stop-list-formlet (route-state-list state)) . => . list-state})
                 (div ([class "three-column-inner"])
                      )
                 (div ([class "three-column-inner"])
                      ))
            (p (input ([type "submit"])))))
      (route-state number type start end list-state null))))

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
                  ,{(preset-text-input
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
    (let* ([state1 (stops-state-list1 state)]
           [state2 (stops-state-list2 state)])
      (formlet
       (#%#
        (div ([class "row"])
             ,{(stop-list-formlet state1) . => . list-state1}
             ,{(stop-list-formlet state2) . => . list-state2}))
       (stops-state list-state1 list-state2))))

  (define (bindings request)
    (force (request-bindings/raw-promise request)))

  (define (has-bindings? request)
    (not (null? (bindings request))))

  (define (route-state-from-request request)
    (if (has-bindings? request)
        (formlet-process (route-formlet (web-state-route (get-global-state)))
                         request)
        (web-state-route (get-global-state))))

  (define (stops-state-from-request request)
    (if (has-bindings? request)
        (formlet-process (stop-formlet (web-state-stops (get-global-state)))
                         request)
        (web-state-stops (get-global-state))))

  (define stylesheet-link `(link ((rel "stylesheet")
                                  (href "/styles.css")
                                  (type "text/css"))))

  (define (create-stop-info-response state embed/url)
    (response/xexpr
     `(html
       (head (title "route21 - Routes Between Stops")
             ,stylesheet-link)
       (body
        ,(tabbing tab-info 0 embed/url)
        (fieldset
         (legend "Stop Selection")
         (form ([action ,(embed/url render-stop-info-page)])
               ,@(formlet-display (stop-formlet state))
               (p (input ([type "submit"])))))
        (fieldset
         (legend "Routes for Selected Stops")
         ,(route-table state))))))

  (define (create-route-edit-response state embed/url)
    (response/xexpr
     `(html
       (head (title "route21 - Routes Between Stops")
             ,stylesheet-link)
       (body
        ,(tabbing tab-info 1 embed/url)
        ,@(formlet-display (route-formlet state embed/url))))))

  (define (render-route-edit-page request)
    (let* ([route-state (route-state-from-request request)]
           [response-generator (lambda (embed/url)
                                 (create-route-edit-response route-state embed/url))])
      (set-global-state (struct-copy web-state
                                     (get-global-state)
                                     [route route-state]))
      (send/suspend/dispatch response-generator)))

  (define (route-table state)
    (let* ([stop1 (stop-list-state-stop (stops-state-list1 state))]
           [stop2 (stop-list-state-stop (stops-state-list2 state))]
           [routes (routes-for-stops stop1 stop2)]
           [route-entries (route-table-entries routes)])
      `(table ,@route-entries)))

  (define (set-global-state new-state)
    (web-cell-shadow global-state new-state)
    #;(printf "new global state:~n~a~n" new-state))

  (define (get-global-state)
    (web-cell-ref global-state))

  (define (render-stop-info-page request)
    (let* ([stops-state (stops-state-from-request request)]
           [response-generator (lambda (embed/url)
                                 (create-stop-info-response stops-state embed/url))])
      (set-global-state (struct-copy web-state
                                     (get-global-state)
                                     [stops stops-state]))
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

  (define tab-info (vector (cons "Stops" render-stop-info-page)
                           (cons "Route" render-route-edit-page)))

  (define (format-tab-header header)
    (format " ~a " header))

  (define (tabbing tab-info selected-index embed/url)
    `(ul
      ,@(for/list ([index (in-naturals)]
                   [tab tab-info])
          `(li ([style "display:inline"])
               ,(if (= index selected-index)
                    (format-tab-header (car tab))
                    `(a ((href ,(embed/url (cdr tab))))
                        ,(format-tab-header (car tab))))))))