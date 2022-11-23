#lang snack/pure (provide emfive:list)

(require
  (only-in "base.rkt" base:#%app base:list) ; Delegated implementations.
  (submod "common.rkt" define-syntax-parser/⊥)) ; Emfive form Infrastructure.

(define-parser emfive:list
  • (_ e:expr ...) #:with [e- ...] (ξ e) (abstractly (§ (base:#%app base:list e- ...))
                                                     (^list (^ e-)))
  • (~is id) (§ base:list))
