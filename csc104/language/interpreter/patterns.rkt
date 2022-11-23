#lang snack/pure (require snack/contract)

(reprovide "eval.rkt")
(provide term/c ≈term/c)

(define term/c syntax?)
(define ≈term/c (or/c term/c error?))

(module* Eval racket/base (provide (all-defined-out))

  (require (submod "..")
           racket/contract snack/contract
           snack/functional snack/definition)

  (splicing-local [(define ≈literal/c (or/c literal? error?)) 
                   (define/contract ReRaise (error? . → . none/c) (∘ raise error-exn))]   

    ; Only for reducers.   
    (define/contract Syntax-Error  (syntax? . → . error?) (∘ Eval syntax->datum)) 
    (define/contract Runtime-Error (any/c #;syntax? . → . error?) Eval #;(∘ Eval syntax->datum))
    ; For unique to stepping: local function escaping.
    (define/contract Error (string? . → . error?) message→error) 
    ;
    (define (⊥/value←≈literal ≈literal) (≈literal/c . → . (not/c ≈term/c))
      ((if (error? ≈literal) ReRaise value←Syntax) ≈literal)) 

    ; For step depending on knowing a user value (repeats number, accumulate sequence).
    ; Only for reducers.
    (define/contract (value←Syntax stx) (literal? . → . (not/c ≈term/c))
      #;(writeln stx)
      ((∘ Eval syntax->datum) stx)) 

    ; non-user-id (including in fn position, possibly hof non-algebraic or hof pass-through by form),
    ;  and void from (block)
    ; Only for reducers.
    (define/contract (Value stx) (syntax? . → . ≈literal/c)
      #;(writeln stx)
      (define result (Eval stx #;(syntax->datum stx)))
      #;(writeln result)
      (if (error? result) result (datum->syntax stx (literal←value result))))))

; ★ — ★

(require (only-in "../forms/patterns.rkt" List)
         (only-in "../forms/forms.app.rkt" emfive:#%app)
         (submod "../without-gui.rkt" interpreter)
         (submod "../functions/non-image.rkt" interpreter))

(require "support.rkt" (multi-in snack {conditional function definition}))

(module* for-fors #false (provide §list build-Call∗ provide-syntax-class))

(reprovide "patterns.define.rkt")

; ★ Recognize/Build List Expression ★

(provide List strip-list-literal)

(define §list §List)
(define strip-list-literal (syntax-parser [(_ e ...) (§list (map strip-context (α e)))]))

; ★ Build Syntactic Call or List ★

(provide build-ct-call)
(define (build-Call∗ f es) (slist* f es))
(define (build-ct-call f es) (slist* #'emfive:#%app f es)) ; ✪ (step (((fun x) x) 1 2))

; ★ Literal ★

(reprovide "literal.rkt")

(provide (rename-out [Reduced •]) Reduced? Literal literal? literal←value)
(define-values (literal? Reduced?) (values (parses? (~is Literal)) (parses? (~is Reduced))))

(provide-syntax-class ⊚ #:attributes {} (ρ (~is-not Reduced)))
(define-syntax-class Reduced #:attributes {}
  (ρ (~is Literal))
  (ρ (~is Define-Function))
  (ρ (~and :Define-Variable (~is (α body) Literal))))

#;
(provide-syntax-class
 Arithmetic-step (ρ (~and (~is Call-step) (~is Arithmetic-literal))))
(define-syntax-class Arithmetic-literal #:attributes {}
  (ρ (~is number))
  (ρ ((~is arithmetic) (~is Arithmetic-literal) ...)))

; ★ Call ★

(provide Call? Call-name
         Call-step
         Functional)

(require (for-template (only-in "../forms/forms.app.rkt" callable))
         syntax/strip-context)
(reprovide (only-in "../forms/forms.binding.rkt" with-local-function))

(splicing-local [(syntax-class Call #:attributes {name} (:callable . _))]
  (define Call? (parses? (~is Call))) 
  (define Call-name (syntax-parser [:Call (α name)])))

; Need to revisit, in particular summarize Alexis' and Ryan's comments in ...
#;https://groups.google.com/d/msg/racket-users/HaSmcTN0SA4/IM6MR_TlAgAJ

(syntax-class Call-step #:attributes {ⓕ [argument 1] §ⓕ∗ §∘arguments §call}
              (:Functional argument ...)
              #:attr §call (λ (es) (build-Call∗ (car es) (cdr es)))
              #:attr §∘arguments (λ (f) (build-Call∗ f (map strip-context (α argument)))))

(syntax-class Functional #:attributes {ⓕ §ⓕ §ⓕ∗} ⓕ
              #:attr §ⓕ (λ  es  (build-Call∗ (α ⓕ) es))
              #:attr §ⓕ∗ (λ (es) (build-Call∗ (α ⓕ) es)))
