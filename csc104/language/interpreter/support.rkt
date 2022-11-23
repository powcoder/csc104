#lang snack/pure (provide (all-defined-out))

(reprovide snack/syntax (submod snack/syntax syntax-classes/core))

(require (for-syntax snack/syntax))

(define-macro (parses? pattern:expr ...) (♯syntax-parser [pattern #true] ...))

; Export-only Syntax Class Definitions
; Define a syntax class only to export, not for this module to rely on.
(define-syntax-parser provide-syntax-class
  [(_ id:id id?:id . parts) (§ (begin (define-syntax-class hidden-name . parts)
                                      (define hidden-name? (parses? (~is hidden-name)))
                                      (provide (rename-out [hidden-name id] [hidden-name? id?]))))]
  [(_ id:id . parts) (§ (begin (define-syntax-class hidden-name . parts)
                               (provide (rename-out [hidden-name id]))))])
