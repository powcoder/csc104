#lang snack/pure (provide emfive:anonymous)

; ●   anonymous  (convert function definition) ●

(require ; for  anonymous
  ; Delegated implementations.
  (only-in "base.rkt" base:#%app)
  (only-in "../display-code.rkt" show-anonymized)
  (only-in "current-definitions.rkt" get-anonymized has-anonymized?)
  ; Static errors.
  (for-syntax (only-in (submod "syntax-error.rkt" miscellaneous) ⌢:anonymous:arity))
  ; Emfive form Infrastructure.
  (submod "common.rkt" define-syntax-parser/⊥))

(define-syntax-parser/⊥ emfive:anonymous #:top
  • (_ f:expr) (§ (base:#%app show-anonymized has-anonymized? get-anonymized f))
  • #:arity ⌢:anonymous:arity)

; ●  comprehension  ●

(provide emfive:comprehension)

#;{(map random (list 100 100 100))
   (list-of (random ℯ) : ℯ ∈ (list 100 100 100))
   (map ((fun ℯ) (random ℯ)) (list 100 100 100))
   (map random (list 100 100 100))
   (list-of (- (random ℯ)) : ℯ ∈ (list 100 100 100))
   (map ((fun ℯ) (- (random ℯ))) (list 100 100 100))
   (map (∘ - random) (list 100 100 100))
   (map (• text-join ∘ "!") (list "ant" "bee" "cat"))
   (map ((fun t) (text-join t "!")) (list "ant" "bee" "cat"))
   (list-of (text-join t "!") : t ∈ (list "ant" "bee" "cat"))}
#;(accumulate (list a b) from (list 1 1)
              update (list (+ a b) a)
              each _ (range 10))
#;(accumulate sum from 0
              update (+ n sum)
              each n (range 10))

;  • error messages : arity, location, form (and expand for error?)
;  • output : fresh variable

(begin-for-syntax
  (require racket/lazy-require)
  (lazy-require [(submod "../functions/non-image.rkt" reflection) {map-id?}]))
(require (only-in "forms.binding.rkt" fun-shape)
         (for-syntax (only-in "patterns.rkt" §for/list))
         (only-in "../display-code.rkt" §display-code))

(define-parser emfive:comprehension
  • (_ (♯map:id (~or* :fun-shape f:expr) l:expr)) #:when (map-id? (α ♯map))
  #:with [v:id] #'{~? parameters [ℯ]} #:with element-expr #'{~? body (f v)}
  (§display-code (§for/list this-syntax (α element-expr) (α v) (α l))))

; ● Sequential ●

(require snack/definition snack/port snack/string
         (only-in "base.rkt" base:#%app)
         (only-in "../../shared/code.rkt" simple-code simply)
         (only-in "current-definitions.rkt" get-parts has-parts?)
         (only-in racket/list first second third fourth)
         (for-syntax (submod "syntax-error.rkt" miscellaneous))
         "forms.binding.rkt")

; • more code introspection
(defines [(show-part has-part? part-getter parts-getter symbolic-name)
          (cond [(has-part? symbolic-name)
                 (define part (part-getter (parts-getter symbolic-name)))
                 (cond [(list? part) (for-each (λ (id) (print id) (display " ")) part)
                                     (newline)]
                       [else part])]
                [else (no-function-definition symbolic-name)])]
  #:with [(no-function-definition symbolic-name)
          (displayln (~a␣ "code:body: needed the name of a function from a function definition,"
                          "but received"
                          "" symbolic-name))])

(provide ex st compthink:sequential)

(require (only-in "../interpreter/patterns.ast.rkt" literal?)
         (for-syntax (only-in (submod "syntax-error.rkt" miscellaneous) ⌢:sequential:arity)))

(define-syntax-parser/⊥ compthink:sequential #:top
  • (_ e:expr) #:attr e- (ξ e) (§∘ (base:#%app simple-code-sequence '#,(unrolled (α e-))))
  • #:arity ⌢:sequential:arity)

(define-parser st
  • (_ e:expr) #:attr e- (abstract (ξ e))
  (§∘ (parameterize [(current-namespace
                      (variable-reference->namespace (#%variable-reference)))]
        (base:#%app literal? #,(if (syntax? (α e-))
                                   #`'#,(α e-)
                                   (datum->syntax #'here (α e-)))))))
(define-parser ex
  • (_ e:expr) #:attr e- (abstract (ξ e))
  #;{(§ '  e-)
     (§ '#'e-)
     (§  #'e-)
     (serialize (α e-))}
  (datum->syntax #'here (α e-)))

(begin-for-syntax

  (require snack/match (except-in snack/list §))

  (struct variable (number) #:transparent)
  
  (define (nested-call stx)
    (match (abstract stx)
      [(^call f arguments) (^call (nested-call f) (map nested-call arguments))]
      [(^literal datum) datum]
      [abs abs]))

  (define (unrolled e)
    (define-values (next-index definitions atomic) (unroll (nested-call e) 0))
    (match definitions
      ['() (list atomic)]
      [`(,definition ... (define ,_ ,e)) (append definition (list e))]))
  
  (define (unroll e next-index)
    (match e
      [(^call f arguments)
       (for/fold ([next-index next-index]
                  [definitions '()]
                  [atomics '()]
                  #:result (let ([r (variable next-index)])
                             (values (add1 next-index)
                                     (append definitions (list (^definition r #false atomics)))
                                     r)))
                 ([a (list* f arguments)])
         (define-values (next-index′ definitions′ atomic) (unroll a next-index))
         (values next-index′ (append definitions definitions′) (append atomics (list atomic))))]
      [e (values next-index '() e)])))

(define (simple-code-sequence codes) (apply values (map (λ (c) (if (list? c) (simple-code c) c))
                                                        codes)))
