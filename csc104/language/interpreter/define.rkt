#lang racket/base (require snack/contract)

(provide with-locals
         (contract-out [♯binding (identifier? . → . (♯ term/c))]
                       [definitions (parameter/c definitions/c)]
                       [substitute ((listof identifier?) (listof term/c) term/c . → . term/c)]))

(define-values (compound? compound-map) (values stx-list? (∘ syn stx-map)))
(define definitions/c (listof Define?))

(require (only-in "patterns.rkt" Define? Define-name term/c)
         (only-in snack/list find-first) snack/functional
         snack/syntax (submod snack/syntax syntax-classes/core))

; Aliasing here is in the static binding sense, not compthink runtime definition.
; In other words match unless shadowed by a local binding.
#;((step (define (- x) (+ x)) (- 2) -) 3) ; ✪ problem, not recognized as shadowed
(define (♯binding an-id) (find-first (λ• (alias? an-id (Define-name •))) (reverse (definitions))))

(define definitions (make-parameter '()))

(define-macro (with-locals some-definitions:expr body:expr ...+) 
  (parameterize ([definitions (append (definitions) some-definitions)])
    body ...))

(define (substitute parameters arguments body)
  (for/fold ([body body]) ([parameter parameters] [argument arguments])
    ((sub parameter argument) body)))

; ✪ Blind substitution, assumes we've restricted ourselves to when that is correct.
(define sub (invariant-assertion (identifier? term/c . → . (term/c . → . term/c))
                                 (λ (an-id replacement)
                                   (λ (into)
                                     (cond [(and (identifier? into) (alias? an-id into))
                                            replacement]
                                           [(compound? into)
                                            (compound-map (sub an-id replacement) into)]
                                           [else into])))))
