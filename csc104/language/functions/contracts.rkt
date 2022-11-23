#lang snack/pure

(provide (struct-out /C) (all-defined-out))

(require "C.rkt"

         (submod "../../english.rkt" types)
         (only-in "custom.rkt" non-negative?)

         snack/syntax)

(define number? real?)

#| Any |#
(define any/C (/C (λ (_) #true) ""))

#| Main Disjoint Patitioning of Types, except boolean and image. |#
(define number/C   (/C real?      type:number))
(define text/C     (/C string?    type:text))
(define function/C (/C procedure? type:function))
(define list/C     (/C (λ (•) (or (pair? •) (null? •))) type:list))

#| Standard Numeric Sub-Types |#
(define-values (integer/C natural/C positive-natural/C)
  (values (/C exact-integer?             type:integer)
          (/C exact-nonnegative-integer? type:natural-number)
          (/C exact-positive-integer?    type:positive-natural-number)))
(generate {[non-negative/C non-negative?           type:non-negative-number]
           [non-zero/C     (λ (•) (not (zero? •))) type:non-zero-number]
           [positive/C     positive?               type:positive-number]}
          [protected/C:id c:expr message]
          (define protected/C (/C (λ (•) (and (number? •) (c •))) message)))

(module* non-image #false (provide (all-defined-out))

  (require (prefix-in message: (submod "../../english.rkt" problems))
           (only-in "custom.rkt" character? unary? binary?))

  (define-values (⧀ “⧀”) #;(values <= message:“≤”) (values < message:“<”))
  
  (define non-empty/C (/C pair?
                          (message:list-length-at-least 1)))
  (define two/C       (/C (λ (•) (and (pair? •)
                                      (pair? (cdr •))))
                          (message:list-length-at-least 2)))
  (define three/C     (/C (λ (•) (and (pair? •)
                                      (pair? (cdr •))
                                      (pair? (cddr •))))
                          (message:list-length-at-least 3)))
  (define four/C      (/C (λ (•) (and (pair? •)
                                      (pair? (cdr •))
                                      (pair? (cddr •))
                                      (pair? (cdddr •))))
                          (message:list-length-at-least 4)))
  (define boolean/C  (/C boolean? type:boolean))

  #| Text-related Sub-Types |#
  (define character/C (/C character? type:character))
  (define unicode/C
    (/C (λ (•) (and (exact-nonnegative-integer? •) (or (<= • 55295) (<= 57344 • 1114111))))
        type:unicode))

  #| Function Sub-Types |#
  (define-values (unary/C binary/C)
    (values (/C (λ (•) (and (procedure? •) (unary?  •))) type:unary-function)
            (/C (λ (•) (and (procedure? •) (binary? •))) type:binary-function)))
  (define-values (unary-predicate/C binary-predicate/C)
    (values (/C (λ (•) (and (procedure? •) (unary?  •))) type:unary-function)
            (/C (λ (•) (and (procedure? •) (binary? •))) type:binary-function)))

  (define-values (list-index/C text-index/C)
    (values
     (/C (λ (collection i) (i . ⧀ .        (length collection)))
         (message:list-index-too-large “⧀”))
     (/C (λ (collection i) (i . ⧀ . (string-length collection)))
         (message:text-index-too-large “⧀”))))

  (define list-lower/C (/C (λ (collection i j) (<= i (length collection)))
                           message:list-range-index-too-large))
  (define list-upper/C (/C (λ (collection i j) (<= j (length collection)))
                           message:list-range-index-too-large))
  (define list-order/C (/C (λ (collection i j) (<= i j))
                           message:number-out-of-order))
  (define text-lower/C (/C (λ (collection i j) (<= i (string-length collection)))
                           message:text-range-index-too-large))
  (define text-upper/C (/C (λ (collection i j) (<= j (string-length collection)))
                           message:text-range-index-too-large))
  (define text-order/C (/C (λ (collection i j) (<= i j))
                           message:number-out-of-order))

  (define small-enough-for-random/C
    (/C (λ (•) (and (exact-positive-integer? •) (• . < . 4294967088))) type:smaller-number)))

(module* image #false (provide (all-defined-out))
  
  (require (prefix-in message: (only-in (submod "../../english.rkt" problems)
                                        ≤-something-of-the
                                        length-of-color-list))
           (only-in "image-implementation.rkt" image? color? colors?)
           (only-in racket/math exact-round)
           (only-in 2htdp/private/image-more image-width image-height)
           snack/definition)
  
  (define-values (image/C color/C colors/C) (values (/C image?  type:image)
                                                    (/C color?  type:color-type)
                                                    (/C colors? type:list-of-colors)))

  (define angle/C (/C number? type:angle))
  (define percentage/C (/C (λ (•) (and (number? •) (<= 0 • 100))) type:percentage))

  (define font-size/C (/C (λ (•) (and (number? •) (<= 1 • 255))) type:font-size))

  (define-values (within-width/C within-height/C)
    (values
     (/C (λ (image amount) ((exact-round amount) . <= . (image-width  image)))
         (message:≤-something-of-the type:width  type:image))
     (/C (λ (image amount) ((exact-round amount) . <= . (image-height image)))
         (message:≤-something-of-the type:height type:image))))

  (define pixel-dimensions/C (/C (λ (colors width height) (= (length colors) (* width height)))
                                 message:length-of-color-list)))
