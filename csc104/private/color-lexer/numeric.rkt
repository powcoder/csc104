#lang racket/base (provide inexact-bad
                           decimal        decimal-bad
                           binary         binary-bad
                           hexadecimal    hexadecimal-bad
                           special-number special-number-bad
                           digit16)

(require (only-in "configuration.rkt" :#)
         "support.rkt" parser-tools/lex (prefix-in : parser-tools/lex-sre))

(define-lex-abbrevs

  [special-number (:or "NaN" "+infinity" "-infinity" "+∞" "-∞")]

  [inexact (:# "i")]
  [sign (char-set "+-")]
  [⊙ "."]

  [radix2  (:# "b")]
  [radix10 (:# "d")]
  [radix16 (:# "x")]

  [digit2  (:/ "01")]
  [digit10 (:/ "09")]
  [digit16 (:/ "af" "AF" "09")]

  [exponent-marker (char-set "eE")]

  [special-number-bad (:# (:or (⫋ "NaN") (⫋ "+infinity") (⫋ "-infinity") (⫋ "+∞") (⫋ "-∞")))]

  [hexadecimal-bad radix16]

  [inexact-bad (:: inexact (:? (:#)))]

  [decimal-bad (:: (:? inexact) (:or (:: (:? sign) (:+ digit10) "/")
                                     (:: (:or radix10
                                              (:: (:? radix10)
                                                  (:or sign
                                                       (:: (:? sign)
                                                           (:or ⊙ (scientific-bad digit10)))))))))]
  
  [binary-bad (:: (:? inexact) radix2 (:? sign) (:? ⊙) (:? (scientific-bad digit2)))])

(define-lexer-pattern (mantissa digit) (:or (:: (:+ digit) (:? ⊙))
                                            (:: (:* digit)     ⊙  (:+ digit))))

(define-lexer-pattern (scientific-bad digit)
  (:: (mantissa digit) (:: exponent-marker (:? sign))))

(define-lexer-pattern (real digit)
  (:: (:? sign) (mantissa digit) (:? (:: exponent-marker (:? sign) (:+ digit)))))

(define-lex-abbrevs

  (hexadecimal (:: radix16 (:+ digit16)))
  
  (binary  (:: (:? inexact) radix2 (real digit2)))

  (decimal (:: (:? inexact) (:or (:: (:? sign) (:+ digit10) "/" (:+ digit10))
                                 (:: (:? radix10) (real digit10))))))
