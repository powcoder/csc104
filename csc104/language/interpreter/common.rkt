#lang racket/base

#| Common to interpreter.rkt, layout.rkt, and now step-display.rkt |#
(provide (struct-out markup)
         (struct-out new)
         (struct-out next)
         cleanse)

(struct markup (expression) #:transparent)
(struct new  markup () #:transparent)
(struct next markup () #:transparent)

(require snack/conditional)
(define (cleanse expression)
  (type-case expression
    [markup? (cleanse (markup-expression expression))]
    [list?   (map cleanse expression)]
    [else    expression]))

#| Uses of Image Functions in csc104/interpreter Tree |#
(require (only-in (combine-in 2htdp/private/image-more mrlib/image-core)
                  image?
                  image-width rectangle
                  image-height above overlay color))

(module* kinds #false (provide image?))

(module* token #false (provide image? image-width rectangle
                               image-height overlay color above)
  ; And csc104/step.rkt gets what it needs from:
  #;(submod "interpreter/token.rkt" step-form))

(module* strings-and-images #false (provide image? image-width rectangle above))
