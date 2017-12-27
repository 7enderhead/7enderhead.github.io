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
    (printf "filter-defs ~a -> ~a~n" state filter-layout)
    filter-layout))

(define (stop-formlet state)
  (let* ([state1 (stop-formlet-state-list1 state)]
         [current-stop1 (stop-list-state-stop state1)]
         [layout1 (stop-list-state-layout state1)]
         [stops1 (filter-stops stops (filter-defs state1))])
    (formlet
     (#%#
      (h2 ,(if current-stop1
               (stop-name current-stop1)
               "no stop selected"))
      (h3 ,(if current-stop1
               (format "(~a / ~a)"
                       (format-range (lon-range current-stop1))
                       (format-range (lat-range current-stop1)))
               "-"))
      (div ,{(stop-list-input stops1 current-stop1) . => . selected-stops1})
      (div ,{(checkbox-input (stop-list-state-use-name-filter? state1))
             . => . use-name-filter1?}
           "Name filter "
           ,{(default-text-input
               (list-layout-filter-expr (stop-list-state-layout state1)))
             . => . name-filter1})

      (div ,{(checkbox-input (stop-list-state-use-lon-filter? state1))
             . => . use-lon-filter1?}
           "Longitude filter")
      (div
       "min. Lon.: "
       ,{(number-input (list-layout-min-lon layout1) min-lon max-lon min-lon)
         . => . min-lon1})
      (div
       "max. Lon.: "
       ,{(number-input (list-layout-max-lon layout1) min-lon max-lon max-lon)
         . => . max-lon1})

      (div ,{(checkbox-input (stop-list-state-use-lat-filter? state1)) . => . use-lat-filter1?}
           "Latitude filter")
      (div
       "min. Lat.: "
       ,{(number-input (list-layout-min-lat layout1) min-lat max-lat min-lat)
         . => . min-lat1})
      (div
       "max. Lat.: "
       ,{(number-input (list-layout-max-lat layout1) min-lat max-lat max-lat)
         . => . max-lat1}))
     
     (stop-formlet-state
      (stop-list-state (if (not (null? selected-stops1))
                           (car selected-stops1)
                           #f)
                       (list-layout name-filter1 min-lon1 max-lon1 min-lat1 max-lat1 0)
                       use-name-filter1? use-lon-filter1? use-lat-filter1?)
      null))))

(define (bindings request)
  (force (request-bindings/raw-promise request)))

(define (has-bindings? request)
  (not (null? (bindings request))))

(define (render-stop-info-page request)
  (define new-state (if (has-bindings? request)
                        (formlet-process (stop-formlet (web-cell-ref formlet-state)) request)
                        default-stop-formlet-state))
  (web-cell-shadow formlet-state new-state)
  (define (response-generator embed/url)
    (response/xexpr
     `(html
       (head (title "Stop Info"))
       (body (h1 "List of Stops")
             (form ([action ,(embed/url render-stop-info-page)])
                   ,@(formlet-display (stop-formlet new-state))
                   (p (input ([type "submit"]))))
             ))))
  (send/suspend/dispatch response-generator))