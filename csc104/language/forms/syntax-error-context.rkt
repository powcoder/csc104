#lang snack/pure (require racket/contract/base)

(provide (contract-out [current-syntax-error ([string?] [any/c #:implicit symbol?] . ->* . any)]
                       [context (parameter/c syntax? (or/c #false syntax?))]))

(require (submod snack/syntax syntax-classes))

(define context (make-parameter #false))
(define (current-syntax-error msg [detail #false] #:implicit [♯name #false])
  (syntax-error ♯name (context) msg detail))
(define (syntax-error ♯name stx msg detail)
  (raise-syntax-error
   (or ♯name (syntax-parse stx [((form:id . _) . _) (syntax-e (attribute form))] [_ #false]))
   msg stx detail))

; this-syntax : lexically bound to syntax in syntax-parses and syntax-classes

; current-syntax-context : parameter defaulting to #false, that we can set for wrong-syntax

; racket/stxparam ?

#;{(syntax-local-context) → (or/c 'expression 'top-level 'module 'module-begin list?)}
#;{(syntax-local-phase-level) → exact-integer?}
#;{(syntax-transforming?) → boolean?}
#;{(syntax-local-name) → any/c}
#;{(syntax-transforming-module-expression?) → boolean?}
