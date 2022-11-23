#lang racket/base (require snack/contract)

(provide (contract-out [setup-config-panel
                        ((is-a?/c area-container<%>) . → . (case-> (→ (listof boolean?))
                                                                   (any/c . → . void?)))]))

(require (prefix-in message: (submod "../words/ui.rkt" settings-panel))
         (only-in racket/gui/base area-container<%> vertical-panel% group-box-panel% check-box%)
         (submod snack/class use) snack/gui
         snack/lambda)

(define (setup-config-panel parent)
  (send parent set-alignment 'center 'center)
  (define prefix-types-check-box
    (with-parent parent (with-parent (new vertical-panel%
                                          [alignment '(center center)]
                                          [stretchable-width  #false]
                                          [stretchable-height #false])
                          (with-parent (new group-box-panel%
                                            [label message:output-style]
                                            [alignment '(left center)])
                            (new check-box%
                                 [label message:prefix-type]
                                 [callback void])))))
  (case-λ [() (list (send prefix-types-check-box get-value))]
          [(v)      (send prefix-types-check-box set-value v)]))
