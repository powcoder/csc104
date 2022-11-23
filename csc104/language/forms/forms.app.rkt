#lang snack/pure (provide emfive:#%app (for-syntax callable))

(require
  ; Delegated implementations.
  (only-in "base.rkt" base:#%app base:let)
  (only-in "forms.binding.rkt" #%fun)
  ; Shared syntax classes.
  (only-in "forms.binding.rkt" fun-shape)
  (for-syntax (only-in (submod "syntax-error.rkt" call) ⌢:call:nullary ⌢:call:non:id/fun)
              racket/lazy-require)
  ; Emfive form Infrastructure.
  (submod "common.rkt" define-syntax-parser/⊥)
  ; Generic.
  (for-syntax (multi-in snack {function boolean conditional})))

(begin-for-syntax
  ; Here : — . Interpreter patterns : name .
  (syntax-class callable #:attributes {name} (~or* name:id :fun-shape (name:id _ ...+)))
  (lazy-require [(submod "../functions/image.rkt"     nullary) (image:nullary-id?)]
                [(submod "../functions/non-image.rkt" nullary) (non-image:nullary-id?)])  
  (define id-nullary? (∨ image:nullary-id? non-image:nullary-id?)))

(define-syntax-parser/⊥ emfive:#%app  
  ; Special Non-Call Forms
  • (_ . fun:fun-shape) (§ (#%fun . fun))
  ; Call Forms  
  ;  — has an f, is f even a valid expression
  • (_ . (f . _)) #:do [(expand-for-errors (α f))] #:when #false '⊥
  ;  — has valid callable f, call's form ok except error for nullary form of non-whitelisted f
  • (_ . (f:callable argument:expr ...))
  (ifs • ((null? (α argument)) . ⇒ . ((∧ identifier? id-nullary?) (α f)))
       (define/syntax-parse [f- argument- ...] (expand-for-structure∗ (list* (α f) (α argument))))
       (abstractly (§ (base:let ([f-value f-])
                                (if #true #;(base:#%app procedure? f-value)
                                    (base:#%app f-value argument- ...)
                                    (void))))
                   (^call (^ f-) (^ argument-)))
       • else (⌢:call:nullary (α f)))
  ;  — no f, uncallable f, or non-calling form (e.g. dotted, keywords)
  • (_ . (~maybe (part . _))) (⌢:call:non:id/fun (or (α part) (void))))
