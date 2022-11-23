#lang snack/pure

#| Connect to language via namespace reflection |#

; Usage within this module is to determine if an identifier is bound to a function,
;  where the function is not being managed by the stepper's environment (need to
;  describe details of “managed”).

(module eval racket/base (provide Eval (struct-out error) whole-error? message→error)

  (require snack/definition)

  (define (Eval r) (with-handlers ([exn:fail? whole-error]) (eval r)))
  
  (struct error (exn expression) #:transparent) #;{exn:fail? any/c}
  
  (defines
    [(whole-error exn) (error exn ∅)]
    [(whole-error? step) (and (error? step) (equal? (error-expression step) ∅))]
    [(message→error message) (whole-error (exn:fail message (current-continuation-marks)))]
    #:with [∅ #'⌢]))

(provide ≈syntax/c)
(reprovide 'eval)

(define ≈syntax/c (or/c syntax? error?))

#| Literal |#

(provide literal?
         literal←value ; Only for reducers.
         provide ⊚ (rename-out [reduced? •]))

(require "../forms/ast.rkt"
         (only-in (submod "common.rkt" kinds) image?)
         (multi-in snack {match function}))

(define datum? (∨ boolean? string? real? image?)) ; Non-list/function values

(define (literal? e) (if (symbol? e)
                         (procedure? (Eval e))
                         (match e
                           [(^literal datum) (datum? datum)]
                           [(? ^fun?) #true]
                           [(^list elements) (andmap literal? elements)]
                           [_ #false])
                         #;{(ρ (~is Else))
                            (ρ ((~is functional) (~is Literal) ...))}))

(define (literal←value value) #;((not/c ≈syntax/c) . → . literal?)
  (type-case value
    [datum? (^literal value)]
    [procedure? (object-name value)] ; ✪
    [list? (^list (map literal←value value))]
    [else 'problem]))

(define reduced? literal?
  #;{(ρ (~is Define-Function)) (ρ (~and :Define-Variable (~is (α body) Literal)))})

(define ⊚ (¬ reduced?)) ; export-only

; ———

(require (only-in "../forms/forms.app.rkt" emfive:#%app)
         (submod "../functions/non-image.rkt" interpreter))

(require "support.rkt"
         snack/contract snack/conditional snack/function)

(module* for-fors #false (provide build-List build-Call∗ provide-syntax-class))

(reprovide "patterns.define.rkt")

; Recognize/Build List Expression
; ———————————————————————————————

(provide ^list?)

; For marshalling lists from racket, folding forms, and map and repeats whose step produces a list.
(define build-List ^list)



; Build Syntactic Call
; ————————————————————

(define (build-Call∗ f es) (slist* f es))

(provide build-ct-call)
(define (build-ct-call f es) (slist* #'emfive:#%app f es)) ; ✪ (step (((fun x) x) 1 2))

; ———

(provide-syntax-class Arithmetic-step (ρ (~and (~is Call-step) (~is Arithmetic-literal))))
(define-syntax-class Arithmetic-literal #:attributes {}
  (ρ (~is number)) (ρ ((~is arithmetic) (~is Arithmetic-literal) ...)))

#| Call, HoFs |#
(provide Call-step Functional Map/Combine Repeats Select/Sort H²oF Map Compose Partial Fun)

(require (for-meta -1
                   (only-in "../forms/forms.binding.rkt" fun-shape)
                   (only-in "../forms/forms.app.rkt" callable)))
(require (only-in "../forms/forms.binding.rkt" with-local-function))
(provide with-local-function)

(define-syntax-class Fun #:attributes {name (parameter 1) body} (ρ :fun-shape))

(provide-syntax-class Call #:attributes {name} (ρ (:callable . _)))

(define-syntax-class Call-step #:attributes {ⓕ [argument 1] §ⓕ∗ §∘arguments}
  (ρ (:Functional argument ...) #:attr §∘arguments (λ (f) (build-Call∗ f (α argument)))))

(define-syntax-class Functional #:attributes {ⓕ §ⓕ §ⓕ∗}
  (ρ ⓕ
     #:attr §ⓕ (λ  es  (build-Call∗ (α ⓕ) es))
     #:attr §ⓕ∗ (λ (es) (build-Call∗ (α ⓕ) es))))

; Algebraic, grouped by signature
(define-syntax-class Compose #:attributes {§f∗}
  (ρ (~and (:compose~ 1:Functional 2:Functional) #;(~is Literal))
     #:attr §f∗ ((α value) (α 1.§ⓕ) (α 2.§ⓕ∗))))
(define-syntax-class Partial #:attributes {§f}
  (ρ (~and (:partial~ :Functional b) #;(~is Literal))
     #:attr §f ((α value) (α §ⓕ) (α b))))
(define-syntax-class Map/Combine #:attributes {ϕf} (ρ :Map) (ρ :combine~ #:attr ϕf (α value)))
(define-syntax-class Map     #:attributes {ϕf} (ρ :map~     #:attr ϕf (∘ build-List (α value))))
(define-syntax-class Repeats #:attributes {ϕf} (ρ :repeats~ #:attr ϕf (∘ build-List (α value))))
(define-syntax-class H²oF #:attributes {cross? ϕF}
  (ρ :cross~    #:attr ϕF (α value) #:attr cross? #true)
  (ρ :parallel~ #:attr ϕF (α value) #:attr cross? #false))
; Marshalled
(define-syntax-class Select/Sort #:attributes {unary? §ⓕ}
  (ρ (~and (~is only~) :Functional) #:attr unary? #true)
  (ρ (~and (~is sort~) :Functional) #:attr unary? #false))

; ●  else  ●
(require (for-meta -1 (only-in "../forms/forms.conditional.rkt" else-binding)))
(provide Else)
(syntax-class Else #:attributes {} (~is else-binding))
