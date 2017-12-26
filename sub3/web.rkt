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
(require "data-defs.rkt")
(require "data-provider.rkt")
(require "data-provider-factory.rkt")
(require "list-layout.rkt")
(require "stop-formlet-state.rkt")

;; debug imports
(require web-server/http/request-structs)

(define info (get-info/full "."))

(define provider (data-provider))

(define stops (~> (send provider stops)
                  (sort (lambda (stop1 stop2)
                          (string<? (stop-name stop1)
                                    (stop-name stop2))))))

(define default-stop-list-state
  (stop-list-state #f
                   (list-layout ""
                                (min-lon stops)
                                (max-lon stops)
                                (min-lat stops)
                                (max-lat stops)
                                0)
                   #t #f #f))

(define default-stop-formlet-state
  (stop-formlet-state default-stop-list-state
                      default-stop-list-state))

(define formlet-state
  (make-web-cell default-stop-formlet-state))

(define (start request)
  (render-stop-info-page request))

(define (stop-formlet state)
  (printf "** formlet given state:~v~n" state)
  (let* ([state1 (stop-formlet-state-list1 state)]
         [current-stop1 (stop-list-state-stop state1)]
         [layout1 (stop-list-state-layout state1)]
         [stops1 (filter-stops stops layout1)])
    (formlet
     (#%#
      (h2 ,(if current-stop1
               (stop-name current-stop1)
               "no stop selected"))
      (div ,{(multiselect-input stops1
                                #:multiple? #f
                                #:attributes (list
                                              (list 'size
                                                    (->string (info 'web-stop-list-size))))
                                #:display (lambda (stop)
                                            (stop-name stop))
                                #:selected? (lambda (stop)
                                              (equal? stop current-stop1))) . => . selected-stops1})
      (div ,{(cross (pure (lambda (x) (and x #t)))
                    (checkbox "" (stop-list-state-use-name-filter? state1))) . => . use-name-filter1?}
           "Name filter "
           ,{(to-string (default #"" (text-input
                                      #:value (list-layout-filter-expr (stop-list-state-layout state1))))) . => . name-filter1})
      (div ,{(input #:type "range") . => . min-lon}))
     (stop-formlet-state (stop-list-state (if (not (null? selected-stops1))
                                              (car selected-stops1)
                                              #f)
                                          (list-layout name-filter1 0 100 0 100 0)
                                          use-name-filter1? #f #f)
                         null))))

(define (bindings request)
  (force (request-bindings/raw-promise request)))

(define (has-bindings? request)
  (not (null? (bindings request))))

(define (render-stop-info-page request)
  (printf "render request bindings: ~a~n" (bindings request))
  (define new-state (if (has-bindings? request)
                        (formlet-process (stop-formlet (web-cell-ref formlet-state)) request)
                        default-stop-formlet-state))
  (printf "render new state: ~a~n" new-state)
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