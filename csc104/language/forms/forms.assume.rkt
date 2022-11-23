#lang snack/pure (provide compthink:need!)

; Delegated implementations.
(require (only-in "../../shared/code.rkt" syntax→code simple-code))
(define (printable-assertion expression)
  (define representation (syntax→code expression))
  (if (list? representation) (simple-code representation) representation))
(define (true? v) (eq? v #true))

(require
  ; Delegated implementations.
  (only-in "base.rkt" base:if base:#%app base:print)
  (only-in "forms.binding.rkt" close)
  ; Static errors.
  (for-syntax (only-in (submod "syntax-error.rkt" conditionals) ⌢:assuming:arity))
  ; Emfive form Infrastructure.
  (submod "common.rkt" define-syntax-parser/⊥))

(define-syntax-parser/⊥ (compthink:need! pre-condition:expr result:expr)
  (§∘ (base:if (base:#%app true? pre-condition)
               result
               (base:#%app base:print (base:#%app printable-assertion #,(close (§ pre-condition))))))
  • #:arity ⌢:assuming:arity)

