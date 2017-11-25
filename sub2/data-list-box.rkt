#lang racket

(require racket/gui/base)

(define data-list-box%
  (class list-box%

    (init label
          parent
          choices
          [meta-data #f])

    (super-new [label label]
               [parent parent]
               [choices choices])

    (define the-meta-data meta-data)

    (define/public (get-meta-data) the-meta-data)

    (define/public (set-meta-data new-meta-data)
      (set! the-meta-data new-meta-data))

    (define/public (set-column-widths . widths)
      (for ([index (in-naturals)]
            [width widths])
        (cond
          ((list? width) (send/apply this set-column-width index width))
          (else (send this set-column-width index width width width)))))))

(provide (all-defined-out))