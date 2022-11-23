#lang racket/base (provide (all-defined-out))

(require parser-tools/lex (prefix-in : parser-tools/lex-sre)
         snack/symbol
         snack/syntax)

(define-macro (define-lexer-pattern (name:id part ...) template)
  (define-lex-trans name (syntax-parser [(_ part ...) #'template])))

(define-lex-trans ⫋
  (syntax-parser [(_ s:string) #:when ((string-length (syntax-e #'s)) . <= . 1) #'nothing]
                 [(_ s:string) #:with s′
                               (substring (syntax-e #'s) 0 (sub1 (string-length (syntax-e #'s))))
                               #'(:or (⫋ s′) s′)]))

; Based on the docs and implementation of racket-lexer :
(define-symbols #:prefix type:
  'error
  'comment 'sexp-comment 'white-space 'constant 'string 'no-color
  'parenthesis 'hash-colon-keyword 'symbol 'eof #;'other)

(define (ret lexeme type paren start-pos end-pos)
  (values lexeme type paren (position-offset start-pos) (position-offset end-pos)))

(define (get-offset in)
  (define-values (x y offset) (port-next-location in))
  offset)
