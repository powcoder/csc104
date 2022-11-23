#lang snack/pure (require snack/contract)

#;(require (only-in racket/base println for-each))

(provide term/c ≈term/c term⊙/c
         reducer/c
         (contract-out (reducer (reducer/c reducer/c . → . (redex? . → . ≈term/c)))
                       (struct redex ([definitions (listof term/c)]
                                      [expression term/c]
                                      [reducer delayed-reduction/c]))
                       (struct Hole ([reduce-thunk delayed-reduction/c]))
                       (struct scope ([locals (listof term/c)]
                                      [context Context?]))
                       (struct Context ([builder #;(term⊙/c ... . → . term⊙/c) ; ✪
                                                 (unconstrained-domain-> term⊙/c)]
                                        [unplugged (listof term⊙/c)]))
                       (struct (skipper reduction) ([λ (reducer/c . → . ≈term/c)]))
                       (struct (peeker  reduction) ([λ (reducer/c . → . ≈term/c)])))
         ⊕)

; A redex contains its reduction as a thunked :
;  • reduced term, or
;  • functional struct needing a  step₁  or  reduced  stepper  variant to produce the reduced term
; This module's  reducer  takes such a  step₁  and  reduced , producing a function to reduce redexes.

(require (only-in "patterns.rkt" ≈term/c term/c) (only-in "context.rkt" term⊙/c)
         "define.rkt"
         (multi-in snack (match syntax)))

(define reducer/c (term/c . → . ≈term/c))

(struct reduction (λ) #:property prop:procedure 0)

(define delayed-reduction/c (→ (or/c ≈term/c reduction?)))

(struct redex (definitions expression reducer) #:transparent)
  
(struct Context (builder unplugged) #:transparent)
(struct Hole    (reduce-thunk)      #:transparent)
(struct scope   (locals  context)   #:transparent)

(struct peeker  reduction ())
(struct skipper reduction ())

(define-macro (⊕ r:expr ... r′:expr) (Hole (λ () r ... r′)))

(define ((reducer step₁ reduced) a-redex)
  #;(let [(e (redex-expression a-redex))] (for-each println (list e (syntax->list e))))
  (match-define (redex ds _ λr) a-redex)
  (with-locals ds
    (define λ/r (λr))
    #;(println λ/r)
    (cond [(peeker?  λ/r) (λ/r step₁)]
          [(skipper? λ/r) (define result (λ/r reduced)) #;(println result) result]
          [else λ/r])))
