#lang racket

(require racket/gui/base)

(define data-list-box% (class list-box%
                           (init label
                                 parent
                                 choices
                                 [columns '("Column")]
                                 [style '(single)]
                                 [callback (lambda (control event) (void))]
                                 [meta-data #f])
                           (super-new [label label]
                                      [parent parent]
                                      [choices choices]
                                      [columns columns]
                                      [style style]
                                      [callback callback])
                           (define the-meta-data meta-data)
                           (define/public (get-meta-data) the-meta-data)
                           (define/public (set-meta-data new-meta-data) (set! the-meta-data new-meta-data))))

(provide (all-defined-out))