#lang snack/pure (provide compthink:#%top-interaction #;compthink:#%datum #;compthink:quote)

(require
  ; Delegated implementations.
  (only-in "base.rkt" base:quote base:#%top-interaction)
  ; Static errors.
  (for-syntax (only-in (submod "syntax-error.rkt" miscellaneous) message:unusable-character))
  ; Emfive form infrastructure.
  (submod "common.rkt" define-syntax-parser/⊥)
  ; Generic.
  (for-syntax snack/definition))

; ●  #%datum  ●
; For emfive should be one of : boolean, real number, string, image struct.
; Note: racket's  number?  is  equivalent to its  complex? .
; ToDo : restrict reader, restrict allowed datums (here or via reader's handling of snips).
;               —capability to lift some/all of those restrictions?
#;(define-syntax-parser compthink:#%datum [(_ . datum) (abstractly (§ (base:quote datum))
                                                                   (^literal (§ datum)))])

; ● Punctuation ● 
#;(define-syntax-parser/⊥ (compthink:quote . parts)
    (local [(define form (form-name this-syntax))]
      (if (= (syntax-span form) 1) (raise-syntax-error '|' | (message:unusable-character) form)
          (§ (base:quote . parts)))))

; ●  #%top-interaction  ●
(define-parser compthink:#%top-interaction
  • (_ (~and our-step (~or* (~datum step) (~datum steps))) . parts)
  (§ (base:#%top-interaction our-step #:interactions . parts))
  • (_ . parts) (§ (base:#%top-interaction . parts)))
