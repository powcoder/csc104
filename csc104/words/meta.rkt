#lang snack/pure

(provide (all-defined-out))
(reprovide reprovide/reprovide racket/require snack/contract
           (multi-in snack {definition string}))

(require racket/require-syntax snack/syntax (for-syntax syntax/strip-context))

(define-require-syntax submods
  (syntax-parser [(_ mod sub:id ...+)
                  (replace-context this-syntax #'(combine-in (submod mod sub) ...))]))

(define-require-syntax snacks
  (syntax-parser [(_ mod:id ...+)
                  (replace-context this-syntax #'(multi-in snack {mod ...}))]))

; • Messager function contracts
(define messager₀/c (→ string?))
(define (messager/c argument/c) (argument/c . → . string?))
(define (messager₂/c argument₀/c argument₁/c) (argument₀/c argument₁/c . → . string?))

(define-syntax-parser strings
  [(_ (~optional (~and prefixed #:prefixed)) [id:id v:expr] ...+
      (~optional (~seq #:with [bind:expr to:expr] ...+)))
   #`(begin (~? (begin (define bind to) ...))
            (define id v) ...
            (provide (prefix-out #,(if (attribute prefixed) #'message: #'||)
                                 (contract-out [id string?] ...))))])

(module print-strings racket/base (provide #%datum quote)
  (require (prefix-in base: (only-in racket/base quote #%datum))
           snack/syntax (for-syntax snack/list))
  (define-syntax-parser quote
    [(_ datum) (for-each println (filter string? (flatten (syntax->datum #'datum))))
               #'(base:quote datum)])
  (define-syntax-parser #%datum
    [(_ . datum:str) (println (syntax->datum #'datum))
                     #'(base:quote datum)]
    [(_ . datum) #'(base:#%datum . datum)]))

