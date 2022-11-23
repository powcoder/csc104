#lang snack/pure

(module forms racket/base
  (provide wrap-λ
           top:checked-identifier
           define:interaction:redefine-check)
  (require (only-in "../../shared/code.rkt" syntax→code)
           snack/definition)
  
  ; ★  fun  anonymous  ★
  (splicing-local [(define anonymous-name '|an anonymous function|)
                   (struct anon (f code)
                     #:property prop:procedure   (struct-field-index f)
                     #:property prop:object-name (λ (self) (syntax→code ((anon-code self)))))]
    (define (wrap-λ a-λ closed-code) (anon (procedure-rename a-λ anonymous-name) closed-code)))
  ; ★ — ★
  ; •  define  #%top  variable-reference
  (require (only-in (submod "../../english.rkt" syntax) define:redefine:user))
  (defines
    [(define:interaction:redefine-check definition id)
     (when (or (identifier-binding id)
               ;; identifier-binding returns #f for variable bound at the top-level, check explicitly:
               (namespace-variable-value (syntax-e id) #true (λ () #false)))
       (syntax-error definition (define:redefine:user id) id))]
    [(top:checked-identifier id)
     (namespace-variable-value
      (syntax-e id) #true (λ () (syntax-error id ⌢:top:checked-identifier:unbound)))]
    #:with
    [⌢:top:checked-identifier:unbound "this variable is undefined"]
    [(syntax-error stx msg [detail #false]) (raise-syntax-error #false msg stx detail)]))



(provide (all-defined-out))

(require (only-in racket/unsafe/ops unsafe-cons-list)
         (only-in racket/list take drop cartesian-product)
         (prefix-in base: (only-in racket/base append sort))
         (only-in "../../shared/code.rkt" printable-value)
         snack/definition)

; ★ HoFs producing a function
(require (only-in racket/list first split-at-right))
(splicing-local [(struct literal-call (ϕ name arguments)    
                   #:property prop:procedure (struct-field-index ϕ)
                   #:property prop:object-name
                   (λ (self) (list* (literal-call-name self)
                                    (map printable-value (literal-call-arguments self)))))]
  (define (compose self-name . .fs)
    (define-values (fs fs-last) (split-at-right .fs 1))
    (define f.0 (first fs-last))
    (literal-call (procedure-reduce-arity-mask (λ args ((apply compose1 fs) (apply f.0 args)))
                                               (procedure-arity-mask f.0))
                  self-name .fs))
  (define (>∘ self-name  . .fs) (literal-call (apply compose1 (reverse .fs)) self-name .fs)) 
  (define (• self-name f . .as) (literal-call (λ (a) (apply f a .as)) self-name (list* f .as))))

(define (>> v . fs) (for/fold ([v v]) ([f fs]) (f v)))

(define (repeats f a n)
  (if (zero? n) '() (let repeats ([a a] [n (sub1 n)] [ℓ (list a)])
                      (cond [(zero? n) (reverse ℓ)]
                            [else (define fa (f a))
                                  (repeats fa (sub1 n) (unsafe-cons-list fa ℓ))]))))

(define (sort comparator ℓ) (base:sort ℓ comparator))
  
(define (cross    hof f l.1 l.2) (hof (λ (l.12) (apply f l.12)) (cartesian-product l.1 l.2)))
(define (parallel hof f l.1 l.2) (hof (λ (l.12) (apply f l.12)) (map list          l.1 l.2)))

(define (identity v) v)
(define (nullary?     f) (procedure-arity-includes? f 0))
(define (unary?       f) (procedure-arity-includes? f 1))
(define (binary?      f) (procedure-arity-includes? f 2))

(define (differ? v₀ v₁) (not (equal? v₀ v₁)))

(define (% c [x 1]) (* c x 1/100))

(define (non-negative? x) (0 . <= . x))
  
(define (element? e ℓ) (and (member e ℓ) #true))
(define (remove   e ℓ) (remove* (list e) ℓ))
(define (sub-list ℓ i j) (take (drop ℓ i) (- j i))) #;ℕ
(define (append ℓ e) (base:append ℓ (list e)))

(define (text->list t) (map string (string->list t))) ; ISL
(define (sub-text? t₀ t) (regexp-match? (regexp-quote t₀) t)) ; ISL

(define (letters? t)    (andmap char-alphabetic? (string->list t)))
(define (whitespace? t) (andmap char-whitespace? (string->list t)))
(define (lower-case? t) (andmap char-lower-case? (string->list t)))
(define (upper-case? t) (andmap char-upper-case? (string->list t)))
(define (digits? t)     (andmap char-numeric?    (string->list t)))
  
(define (character? v) (and (string? v) (= (string-length v) 1)))
(define (character t i) (string (string-ref t i))) #;ℕ
(define (character->unicode c) (char->integer (string-ref c 0)))
(define (unicode->character n) (string (integer->char n)))
