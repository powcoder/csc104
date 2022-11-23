#lang racket/base (require (submod "meta-language.rkt" non-image))

(define-for-syntax default #'racket/base)

(define-syntax-parser create 
  [(_ :contracting binding)
   #:declare binding (alias/rename/from (attribute f) #'here custom)
   (define/provide this-syntax (attribute binding.library) default (attribute binding.data))])

(require (only-in syntax/parse define-literal-set literal-set->predicate))
;
(module* nullary #false (provide (prefix-out non-image: nullary-id?))
  (define-literal-set nullary {+ * text-join join})
  (define nullary-id? (literal-set->predicate nullary)))
;
(module* reflection #false (provide map-id?)
  (define-literal-set map-id {map})
  (define map-id? (literal-set->predicate map-id)))

(define index/C natural/C #;positive-natural/C) #;ℕ

; ★ Build some checks from contracts.rkt contracts via error.rkt combinators

(define check:range (arity-arguments-contract-checker 1 number/C number/C non-zero/C))
(define check:-     (arity-arguments-contract-checker 1 number/C number/C))
(define check:%     (arity-arguments-contract-checker 1 number/C number/C))

(define-values (check-numbers check-texts check-lists check:unaries check-functions)
  (values (arguments-homogeneous-contract-checker number/C)
          (arguments-homogeneous-contract-checker text/C)
          (arguments-homogeneous-contract-checker list/C)
          (arguments-homogeneous-contract-checker unary/C)
          (arguments-homogeneous-contract-checker function/C)))

(define check:function.unaries (checker-fixed-homogeneous `(,function/C) unary/C))

(define-values (check-list-index check-text-index)
  (values (binary-checker list-index/C) (binary-checker text-index/C)))
(define-values (check-list-range check-text-range)
  (values (dependent-argument-checker `(2 ,list-lower/C) `(3 ,list-upper/C) `(3 ,list-order/C))
          (dependent-argument-checker `(2 ,text-lower/C) `(3 ,text-upper/C) `(3 ,text-order/C))))

(define check:partial (checker-fixed-homogeneous `(,function/C ,any/C) any/C))
(define check:any.unaries (checker-fixed-homogeneous `(,any/C) unary/C))

(module* interpreter #false (provide (all-defined-out))

  ; For building de novo calls.
  (define §prepender #'prepend)
  
  (require (only-in (submod snack/syntax syntax-classes/core) define-literal-class))

  ; ★ Language functions bindings with special stepper handling
  
  ; Language function aliases refer to distinct functions (so errors can refer to the alias name),
  ;  thus all must be recognized.

  ; Algebraic HoFs
  ; Call on syntactic function(s) and syntactic elements.
  ; For ones producing explicit list, turn result elements into syntactic list.
  (define-literal-class combine~    {combine})
  (define-literal-class map~        {map})
  (define-literal-class repeats~    {repeats})
  (define-literal-class parallel~   {parallel})
  (define-literal-class cross~      {cross})
  (define-literal-class compose~    {compose ∘})
  (define-literal-class partial~    {partial •})
  (define-literal-class pipe~       {pipe >>})

  ; Marshalled HoFs — unary/binary predicate
  (define-literal-class only~ {select})
  (define-literal-class sort~ {sort})

  ; To hide steps for expressions built from numeric literals and calls of the listed functions.
  (define-literal-class arithmetic (+ * - / inc dec))

  ; To recognize calls of some functions as compound literals (doesn't include list constructor).
  (define-literal-class functional {>∘
                                    compose ∘
                                    partial •}))

(require (only-in racket/math pi))
(provide pi (rename-out [pi π]))

(map-macro create

           ; ★ Type Predicate
           ;  image?  is in image.rkt
           [(number?   any/C) #:renames real?]
           [(function? any/C) #:renames procedure?]
           [(boolean?  any/C)]
           [(text?     any/C) #:renames string?]
           [(list?     any/C)]

           ; ★ Function
           [(nullary? function/C) #:custom]
           [(unary?   function/C) #:custom]
           [(binary?  function/C) #:custom]

           ; ★ Equality
           [(same?   any/C any/C) #:renames equal?]
           [(differ? any/C any/C) #:custom]

           ;  ★ Number
           
           [(+ . xs) check-numbers]
           [(* . xs) check-numbers [#:aka × ·]]
           [(- x₀ . xs) check:-]
           [(/ number/C non-zero/C) [#:aka ÷]]
           
           [(inc number/C) #:renames add1]
           [(dec number/C) #:renames sub1]
           
           [(% c . xs) check:% #:custom]

           [(=  x₀ x₁ . xs) check-numbers]
           [(<  x₀ x₁ . xs) check-numbers]
           [(>  x₀ x₁ . xs) check-numbers]
           [(<= x₀ x₁ . xs) check-numbers [#:aka ≤]]
           [(>= x₀ x₁ . xs) check-numbers [#:aka ≥]]
           
           [(minimum x . xs) check-numbers #:renames min]
           [(maximum x . xs) check-numbers #:renames max]
           [(round   number/C) #:renames exact-round   #:from racket/math]
           [(floor   number/C) #:renames exact-floor   #:from racket/math]
           [(ceiling number/C) #:renames exact-ceiling #:from racket/math]

           [(integer?          any/C) #:renames exact-integer?]
           [(natural?          any/C) #:renames exact-nonnegative-integer?]
           [(positive-natural? any/C) #:renames exact-positive-integer?]
           
           [(negative?     number/C)]
           [(zero?         number/C)]
           [(positive?     number/C)]
           [(non-negative? number/C) #:custom]

           [(random small-enough-for-random/C) #:from '#%kernel] #;ℕ

           [(square-of number/C) #:renames sqr #:from racket/math]
           [(power positive/C number/C) #:renames expt]
           [(square-root non-negative/C) [#:aka √] #:renames sqrt]
           
           [(remainder natural/C positive-natural/C)]
           [(quotient  natural/C positive-natural/C)]       
           [(even? integer/C)]
           [(odd?  integer/C)]
           
           [(sin number/C) [#:aka sine]]
           [(cos number/C) [#:aka cosine]]

           ; ★ Text

           [(text-length text/C) #:renames unsafe-string-length #:from racket/unsafe/ops]
           [(text-join . ts) check-texts #:renames string-append]

           ; ℕ
           [(character text/C index/C)         check-text-index #:custom]
           [(sub-text  text/C index/C index/C) check-text-range #:renames substring]
           
           [(sub-text? text/C text/C) #:custom]
           
           [(number->text number/C) [#:aka number→text] #:renames number->string]
           [(text->number text/C)   [#:aka text→number] #:renames string->number]
           
           [(text->list   text/C) [#:aka text→list] #:custom]
           
           [(lower-case?  text/C) #:custom]
           [(upper-case?  text/C) #:custom]
           [(letters?     text/C) #:custom]
           [(digits?      text/C) #:custom]
           [(whitespace?  text/C) #:custom]
           ;
           [(lower-case   text/C) #:renames string-downcase]
           [(upper-case   text/C) #:renames string-upcase]

           [(character? any/C) #:custom]
           [(character->unicode character/C) [#:aka character→unicode] #:custom]
           [(unicode->character natural/C)   [#:aka unicode→character] #:custom]           
           [(alphabet-order? t₀ t₁ . ts) check-texts #:renames string<=?]
           
           ; ★ List — except  list  which is now a function-like form
           
           [(length list/C)]
           
           [(combine       function/C list/C) #:renames apply]
           [(map              unary/C list/C)]
           [(all?   unary-predicate/C list/C) #:renames andmap]
           [(select unary-predicate/C list/C) #:renames filter]
           [(sort  binary-predicate/C list/C) #:custom]
           
           ; ℕ
           [(range x₁ . range-spec) check:range #:from racket/list]
           [(element  list/C index/C)         check-list-index #:renames list-ref]
           [(sub-list list/C index/C index/C) check-list-range #:custom]
           
           [(list-of natural/C any/C) #:renames make-list #:from racket/list]
           
           [(first non-empty/C) #:renames unsafe-car #:from racket/unsafe/ops]
           [(rest  non-empty/C) #:renames unsafe-cdr #:from racket/unsafe/ops]
           [(second two/C)   #:renames cadr]
           [(third  three/C) #:renames caddr]
           [(fourth four/C ) #:renames cadddr]
           [(empty? list/C)  #:renames null?]
           
           [(join . ℓs) check-lists #:renames append]
           [(prepend any/C list/C)#:renames unsafe-cons-list #:from racket/unsafe/ops]
           [(append list/C any/C) #:custom]

           [(remove   any/C list/C) #:custom]
           [(element? any/C list/C) [#:aka ∈] #:custom]
           [(reverse list/C)]
           ;
           [(repeats unary/C any/C natural/C) #:custom]
           
           ; ★ Boolean
           
           [(not boolean/C)]
           
           ; ★ Other
           
           [(identity any/C) #:custom]

           [(url->browser text/C) [#:aka url→browser] #:renames send-url #:from net/sendurl]

           ; ★ Piping
           
           [(>> v f . .fs) check:any.unaries [#:aka pipe] #:custom]
           
           ; ★ HoFs producing a function
           
           [(compose f₀ f₁ . .fs) #;check-functions check:unaries [#:aka ∘] #:custom #:self]
           [(partial f  b  . .bs) check:partial [#:aka •] #:renames •      #:custom #:self]
           [(>∘      f₀ f₁ . .fs) check:function.unaries                   #:custom #:self]
           
           ; ★ Iteration over two lists.
           [(cross    function/C binary/C list/C list/C) #:custom]
           ; ✪ needs contracting that lists are of same length, or build it on compthink's map
           [(parallel function/C binary/C list/C list/C) #:custom])
