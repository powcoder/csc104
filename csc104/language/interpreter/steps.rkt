#lang snack/pure (require snack/contract)

(require (only-in racket/base println))

(provide id:context define:context or/and:context if:context
         for:context for/list:context step:context with:context
         block:context #;arithmetic:context call:context)

(require (submod "../../english.rkt" stepper)
         (only-in "interpreter-shared.rkt" Context? Context scope? scope Hole? ⊕ peeker skipper)
         (submod "patterns.rkt" Eval)
         "patterns.rkt" "patterns.conditional.rkt" "patterns.for.rkt" "patterns.independent.rkt"
         "algebraic.rkt"
         "define.rkt"
         (submod "hide.rkt" implementation))

(require (submod snack/syntax syntax-classes/core) syntax/parse    
         snack/list (except-in (multi-in snack (syntax functional match definition)) §))

(define (♯builds es [post values])
  (match-define (list •s (list ⊚⋯ •s⋯) ...) (split Reduced? es))
  (and (not (empty? ⊚⋯)) (Context (λ es (post (append •s (append-map list* es •s⋯)))) ⊚⋯)))

#;(define maybe-hole (syntax-parser [(~is ⊚) (hole)] [_ this-syntax]))

(define-syntax-parser define-parser #:literals (syntax)
  [(_ id:id pattern:expr (~optional (syntax data:expr)) result:expr ... final:expr)
   #'(define id (syntax-parser [pattern (~? (syntax-parse #'data result ... [_ (⊕ final)])
                                            (~@ result ... final))]
                               [_ #false]))])
(define-syntax-parser build [(~or* _:id (_ . parts))
                             #:with builder (datum->syntax this-syntax 'builder)
                             #'{~? ((α builder) . parts) (α builder)}])
(define-syntax-parser build₁ [(_ {~optional builder′:expr} e:id)
                              #:with builder (datum->syntax this-syntax 'builder)
                              #'(Context {~? builder′ (α builder)} (list (α e)))])


(splicing-local [(define (♯chase an-id)
                   (define ♯b (♯binding an-id))
                   (if (and (Define-Variable? ♯b) (identifier? (Define-value ♯b)))
                       (♯chase (Define-value ♯b))
                       ♯b))
                 
                 (define (name-function-value f-value f-expr)
                   (procedure-rename f-value (if (identifier? f-expr)
                                                 (syntax-e f-expr)
                                                 '|an anonymous function|)))

                 (define ((substitution arguments) f-definition error-representative)
                   (define/syntax-parse (~or* :Fun :Define-Function) f-definition)
                   (if (same-length? (α parameter) arguments)
                       (substitute   (α parameter) arguments (α body))
                       (Runtime-Error error-representative)))]

  ; ★ id
  ; How do we get here?
  ;   What's considered ⊚ for an id, and which spots handle ids themselves?
  ;   What's considered literal?
  (define-parser id:context r:id
    (⊕ (define ♯b (♯chase (α r)))
       (cond [(and (Define? ♯b) (literal? (Define-value ♯b))) (Define-value ♯b)]
             [(Define-Function? ♯b) (Error (message:local:function-escaping (Define-name ♯b)))]
             [else (Value (α r))])))
  
  (define-parser call:context :Call-step
    (or
     #;(♯builds (α argument) (α §ⓕ∗))
     (♯builds (list* (α ⓕ) (α argument)) (α §call))
     (⊕
      (skipper
       (λ (reducer)
         (syntax-parse this-syntax
                
           ; syntactically valid step for any arity,
           ;  desugared errors suffice
           [(:Compose . _) (apply (α §f) (α argument))]
                
           ; syntactically valid step iff semantically valid arity iff unary,
           ;  then desugared errors suffice
           [(:Partial a)   ((α §f) (α a))]

           ; syntactically valid step iff semantically valid arity and second argument type
           ;  iff binary and second argument is a list
           ; desugared errors can be different, but either suffice
           ; pass-through by form means invalid second argument type will cause  map's  check,
           ;  which means first argument will still be checked first
           ;  —  map  is pickier, checks first argument even with empty list
           ;  — could add  map's  check as a warning
           ;  — could add example of call that would fail to  map's  error
           [(:Map     :Functional ℓ:List) ((α ϕf) (α §ⓕ) (α ℓ.element))]                
           [(:Combine :Functional ℓ:List) ((α ϕf) (α §ⓕ) (α ℓ.element))]

           [(:Select/Sort p:Functional ℓ:List)
            (define ϕp (∘ ⊥/value←≈literal reducer (α p.§ⓕ)))
            (Value ((α §ⓕ) (name-function-value (if (α unary?)
                                                    (λ (e)     (ϕp (literal←value e)))
                                                    (λ (e₀ e₁) (ϕp (literal←value e₀)
                                                                   (literal←value e₁))))
                                                (α p.ⓕ))
                           (strip-list-literal (α ℓ))))]

           #;{(>> v f)        →     (f v)}
           #;{(>> v f g ...+) → (>> (f v) g ...+)}
           [(pipe:Pipe §v :Functional) ((α §ⓕ) (α §v))]
           [(pipe:Pipe §v :Functional §g ...+) ((α pipe.§ⓕ∗) (list* ((α §ⓕ) (α §v)) (α §g)))]

           [(:Repeats :Functional §seed :Number) #:when (exact-nonnegative-integer? (α ϕn))
                                                 ((α ϕf) (α §ⓕ) (α §seed) (α ϕn))]
                
           [(:H²oF :Map :Functional ℓ₁:List ℓ₂:List)
            #:when (or (α cross?) (= (α ℓ₁.the-length)
                                     (α ℓ₂.the-length)))
            ((α ϕF) (α ϕf) (α §ⓕ) (α ℓ₁.element) (α ℓ₂.element))]                
                
           [_ (define do-substitution (substitution (α argument)))
              (define build-call-on-arguments (α §∘arguments))
              (define ♯chased (or (and (identifier? (α ⓕ)) (♯chase (α ⓕ))) (α ⓕ)))
              #;(println ♯chased)
              (syntax-parse ♯chased
                [v:Define-Variable (build-call-on-arguments (α v.value))]
                [f:Define-Function
                 #;(println (list (α f.name) (α f.parameter)
                                  (build-call-on-arguments (α f.name))))
                 (do-substitution this-syntax (build-call-on-arguments (α f.name))
                                  #;(with-local-function (α f.name) (α f.parameter)
                                      #false (build-call-on-arguments (α f.name))))]
                [(~is Fun) (do-substitution this-syntax (build-ct-call (α ⓕ) (α argument)))]
                [_ #;(println (α ⓕ))
                   #;(println (build-call-on-arguments (α ⓕ)))
                   #;(println (syntax->list (build-call-on-arguments (α ⓕ))))
                   (Value (build-call-on-arguments (α ⓕ)))])])))))))

(define-parser define:context :Define-step #'body [body:⊚ (build₁ body)] void)

; ●  if  ●
; Without any hiding:
;  • when first condition is non-literal, step just (as reflected in next-underlining) that condition
;  • otherwise step whole (as reflected in next-underlining) if-expression :
;     1.  drop any false-and-consequent pairs
;     2.  ∘ when first of remaining conditions is non-literal, step that condition
;           ∘ otherwise select its consequent/alternative or error if not boolean literal
;             — on error restore false-and-consequents for correct position number in error message
(define-parser if:context :If-step
  #'[[neutral ...] condition₀]
  [[[] (~is ⊚)] #:when (not (if:hide-conditions?)) (build₁ condition₀)]
  (peeker (λ (step₁) (syntax-parse (α condition₀)
                       [(~or* #true (~is Else)) (α result₀)]
                       [(~is Literal) (Syntax-Error (α non-boolean-condition-representative))]
                       [_ (match (step₁ (α condition₀))
                            [(error exn c′) (error exn (build c′))]
                            [c′ (if (if:hide-conditions?) (step₁ (build c′)) (build c′))])]))))
; ●  or  and  ●
(define-parser or/and:context :Or/And-step
  #'[[neutral ...] condition₀]
  [[[] (~is ⊚)] #:when #true #;(not (if:hide-conditions?)) (build₁ condition₀)]
  ; Probably doesn't need a peeker, but maximizing symmetry with  if  for the time being.
  ; Rationale for similarity and difference with  if  needs writing up.
  (peeker (λ (step₁) (syntax-parse (α condition₀)
                       [(~is boolean) this-syntax]
                       [(~is Literal) (Syntax-Error (α non-boolean-condition-representative))]
                       [_ (build (α condition₀)) #;(match (step₁ (α condition₀))
                                                     [(error exn c′) (error exn (build c′))]
                                                     [c′ (build c′)])]))))

#;(define-parser arithmetic:context :Arithmetic-step (⊕ (Value this-syntax)))

(define-parser step:context :Step-step #'e (α e))

(define-parser with:context :With-step
  #'((definition ...) body)
  [([•definition:• ... d:⊚ definition ...] _)
   (scope (α •definition) (build₁ (build (α •definition) (α definition)) d))]
  [([•definition:• ...] body:⊚)
   (scope (α •definition) (build₁ body))]
  (α body))


(splicing-local [(define (for:empty-sequence? e-stx)
                   (case (value←Syntax e-stx) [("" ()) #true] [else #false])) 
                 (define (for:sequence? e-stx) ((∨ string? list?) (value←Syntax e-stx))) 
                 (define (for:split-sequence e-stx)
                   (define v (value←Syntax e-stx))
                   (define-values (s₀ s′) (if (string? v)
                                              (values (substring v 0 1) (substring v 1))
                                              (values (first v)         (rest v))))
  
                   (values (datum->syntax e-stx (literal←value s₀))
                           (datum->syntax e-stx (literal←value s′))))]
  
  (define-parser for/list:context :For/List-step
    #'s [s:⊚ (build₁ s)]
    (skipper
     (λ (reducer) (cond [(for:empty-sequence? (α s)) §empty-list]
                        [else (define-values (s₀ s′) (for:split-sequence (α s)))
                              (§prepend (reducer (substitute (α parameter) (list s₀) (α body)))
                                        (build s′))]))))

  (define-parser for:context :For-step
    #'es [(e ...) #:do [(define b (♯builds (α e) (α builder∗)))] #:when b b]
    (skipper
     (λ (reducer)
       (if (for:sequence? (α s))
           (if (α as)
               (cond [(for:empty-sequence? (α s)) (α a⒮)]
                     [else (define-values (s₀ s′) (for:split-sequence (α s)))
                           (define a ((if (for:hide?) reducer values)
                                      (substitute (α parameter) (list* s₀ (α as)) (α body))))
                           (build s′ a)])
               (Syntax-Error (α accumulator-stub)))
           (Syntax-Error (α sequence-stub)))))))

(define-parser block:context :Block-step
  #'es [(e₀:⊚ e′ ...) (build₁ (λ• (build • (α e′))) e₀)]
  (syntax-parse this-syntax
    [((~is •) ... ⊚e:⊚ e′ ...+) (build (α ⊚e) (α e′))]
    [((~is •) ... e′) (α e′)]
    [() (Value (build))]))
