#lang snack/pure (require snack/contract)


(require (only-in "patterns.rkt" error? whole-error? error-exn error-expression))


(module* step-form #false
  
  (provide hiding
           (contract-out
            [definitions (parameter/c (listof definition/c))] ; Needs review.
            [program-steps (syntax? #;(listof expression/c)
                                    . → . (listof (sequence/c #:min-count 1 step/c)))]))

  (define-values (definition/c expression/c) (values any/c any/c))
  ; The step forms ask to chunk these, expecting neither of ‘next’ and ‘new’ self-nested.
  (define chunkable/c (and/c (not/c error?) any/c)) ; Needs review.
  ; For non-whole-error errors, the step forms ask to chunk the error-expression.
  (define step/c (or/c error? chunkable/c namespace?))

  (require (submod "hide.rkt" interpreter)
           (only-in "define.rkt" definitions)
           (only-in racket/sequence sequence/c)))


(module* step-display #false
  (provide (contract-out
            [error?       predicate/c] ; A step that is/has an error is wrapped in ‘error’.
            [whole-error? predicate/c] ; Whole expression's reduction produced error?
            [error-exn        (error? . → . exn:fail?)] ; First sibling error.
            ; When not ‘whole-error?’, an illustrative expression that can be chunked.
            [error-expression (error? . → . syntax?)])))


(module program-steps racket/base (provide program-steps)
  
  (require "interpreter.implementation.rkt"
           (only-in "annotate.rkt" key:next:cleanse key:next→next)
           (only-in "patterns.rkt" Define?) (only-in "define.rkt" with-locals))
  
  (require racket/require (only-in (multi-in snack {generator stream match list})
                                   join adjoin match generator for/stream))
  
  (define (program-steps program)
    (define-values (previous-step accumulated-definitions) (values #false '()))
    (let loop ([program (syntax->list program)])
      (match program
        [`(,top-level ,program ...)
         (define steps (generator (λ (yield)
                                    (define the-previous-step (values #;cleanse previous-step))
                                    (set! accumulated-definitions
                                          (join (if (Define? the-previous-step)
                                                    (list the-previous-step)
                                                    (list))
                                                accumulated-definitions))
                                    (with-locals (reverse accumulated-definitions)
                                      ((stepper top-level #:top? #true) yield)))))
         (adjoin (for/stream ([step steps])
                   (set! previous-step (key:next:cleanse step))
                   (key:next→next step))
                 (loop program))]
        ['() (list (list (current-namespace)))]))))

(require 'program-steps)
