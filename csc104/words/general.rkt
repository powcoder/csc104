#lang snack/pure (require "meta.rkt" "words.rkt")

(provide (contract-out
          [comma␣       (string? string? . → . string?)]
          [parenthesize (string? ...     . → . string?)]
          [ors  ((list*/c string? (listof string?)) . → . string?)]
          [ands ((list*/c string? (listof string?)) . → . string?)]
          [the        (string? ...     . → . string?)]
          [indefinite ([string?] [#:based-on string?] #:rest (listof string?) . →∗ . string?)]
          [plural     (string? . → . string?)]
          [cardinal ([natural/c] [#:zero string?] . →∗ . string?)]
          [ordinal  (exact-positive-integer? . → . string?)]
          [n-things (natural/c string? . → . string?)]
          [plural-suffix (natural/c . → . (or/c "" "s"))]
          [arity ([natural/c (or/c natural/c +inf.0) string? natural/c]
                  [#:pluralize? boolean?]
                  . →∗ . (list/c string? string?))]
          [code (any/c . → . string?)]))

(strings #:prefixed [ellipsis "..."] [period "."])

(define vowels '(#\a #\e #\i #\o #\u))
(define indefinite-exceptions '("unary" "unique"))
(define “s” "s")
(define “，” ",")
(define-values (“₍” “₎”) (values "(" ")"))
(define cardinals '("one" "two" "three" "four" "five" "six" "seven" "eight" "nine"))
(define ordinals
  '("first" "second" "third" "fourth" "fifth" "sixth" "seventh" "eighth" "ninth" "tenth"))
(define ordinal-endings '("th" "st" "nd" "rd" "th" "th" "th" "th" "th" "th" "th"))

(require snack/match (only-in racket/list range) racket/pretty)

(define (the . ss) (apply ~a␣ “the” ss))
(defines [(indefinite noun-clause₀ #:based-on [based-on noun-clause₀] . ss)
          (apply ~a␣ (a-or-an based-on) noun-clause₀ ss)]
  #:with [(a-or-an what-follows) (if (or (equal? "" what-follows)
                                         (not (memv (string-ref what-follows 0) vowels))
                                         (ormap (λ (•) (string-prefix? what-follows •))
                                                indefinite-exceptions))
                                     “a”
                                     “an”)])
(define (plural s) (~a s “s”))
(define (plural-suffix n) (if (n . = . 1) "" “s”))

(define (parenthesize . s) (~a “₍” (apply ~a␣ s) “₎”))
(define (comma␣ s₀ s₁) (~a␣ (~a s₀ “，”) s₁))
(defines [ors (combiner “or”)] [ands (combiner “and”)]
  #:with [(combiner with) (λ-match [(list e₀) e₀]
                                   [(list e₀     e₁) (~a␣ e₀ with e₁)]
                                   [(list e₀ ... e₁) (comma␣ (apply ~a e₀ #:separator (~a “，” " "))
                                                             (~a␣ with e₁))])])

(define (n-things n thing) (~a␣ (cardinal n #:zero “no”) (~a thing (plural-suffix n))))
(define (cardinal n #:zero [zero “none”])
  (if (n . <= . 9) (list-ref (list* zero cardinals) n) (~a n)))
(define (ordinal n⁺)
  (if (n⁺ . <= . 10) (list-ref ordinals (sub1 n⁺)) (~a n⁺ (list-ref ordinal-endings (modulo n⁺ 10)))))

(defines [(arity m M kind actual #:pluralize? [pluralize? #true])
          (define-values (expect up-to) (if (< actual m) (at-least m M) (at-most m M)))
          (list (~a␣ expect (~a kind (if pluralize? (plural-suffix up-to) "")))
                (if (< actual m) (too-few actual) (too-many actual)))]
  #:with
  [(too-few  actual) (if (= actual 0) “none” (~a␣ “only” (cardinal actual)))]
  [(too-many actual) (cardinal actual)]
  [(interval m M) (ors (map cardinal (range m (add1 M))))]
  [(at-most m M) (values (~a␣ (cond [(zero? M) “no”]
                                    [(zero? m) (~a␣ “at-most” (cardinal M))]
                                    [else (~a␣ “only” (interval m M))]))
                         M)]
  [(at-least m M) (if (< M +inf.0)
                      (values (interval m M) M)
                      (values (~a␣ “at-least” (cardinal m)) m))])

(define (code name) (parameterize ([print-boolean-long-form #true]
                                   [pretty-print-show-inexactness #true]
                                   [pretty-print-exact-as-decimal #true]
                                   [print-as-expression #true])
                      (pretty-format name #:mode 'write)))
