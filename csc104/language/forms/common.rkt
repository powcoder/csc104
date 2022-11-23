#lang snack/pure

(module* define-syntax-parser/⊥ racket/base (require reprovide/reprovide)  
  
  (provide define-syntax-parser/⊥ define-parser
           (all-from-out snack/syntax) (for-syntax (all-from-out (submod ".."))
                                                   (all-from-out snack/syntax)))
  (reprovide (for-syntax (submod snack/syntax syntax-classes/core)))
  
  (require snack/syntax (for-syntax (submod "..")
                                    (submod ".." internal)
                                    snack/syntax))
  
  (define-macro (define-parser macro:id (~seq (~is •) (~and (~is-not •) part) ...+) ...+)
    (define-syntax-parser macro [part ...] ...))
  
  (define-syntax-parser define-syntax-parser/⊥
    [(_ id:id (~optional (~and #:top top-present))
        (~or* :groups
              (~seq (~seq (~is •) (~and (~is-not •) (~not #:arity) (~not #:error-module) part) ...+)
                    ...+))
        (~optional :arity)
        (~optional (~seq #:error-module module:expr)))
     (§ (define-syntax id (let () (~? (local-require module))
                            (parser/⊥
                             (~? [_:⌢non-top 'top-present])
                             [part ...] ...
                             (~? (~@ #:arity ⌢))))))]
    [(_ (id:id . pattern) template directive ...)
     (§ (define-syntax-parser/⊥ id [(_ . pattern) template] directive ... ))]))

(provide §data)
; Recall : doesn't overwrite existing contexts.
(define (§data that-syntax code) (datum->syntax that-syntax code that-syntax))

(module internal racket/base (require reprovide/reprovide)
  (provide groups • arity)
  (reprovide (submod snack/syntax syntax-classes/core))
  (syntax-class groups #:splice #:attributes {(part 2)} [[part ...+] ...+])
  (syntax-class • #:attributes {} (~datum •))
  (syntax-class arity #:splice #:attributes {⌢} [(~optional (~is •)) #:arity ⌢]))

(provide (all-defined-out) (for-syntax arity))
(reprovide "ast.rkt" "syntax-error-context.rkt")

(define (expand-for-errors stx) (local-expand stx 'expression '()))

(define (top?) (or (definitions-top?) (interactions-top?)))
(define (interactions-top?) (member (syntax-local-context) '(top-level)))
(define (definitions-top?)  (member (syntax-local-context) '(module module-begin)))

(require snack/syntax (submod snack/syntax syntax-classes/core)
         racket/lazy-require
         (for-syntax 'internal))

(syntax-class sequence #:attributes {length} (e ...) #:attr length (length (α e)))

(define form-name (syntax-parser [(~is id) this-syntax] [(head . _) (form-name (α head))]))

(define-macro (parser/⊥ (~or* :groups
                              (~seq (~seq (~is •) (~and (~is-not •) (~not #:arity) part) ...+)
                                    ...+))
                        (~optional :arity))
  (λ (stx) (parameterize [(context stx)]
             (local-require (submod "syntax-error.rkt" miscellaneous))
             (syntax-parse stx [part ...] ...
               (~? [(_ . :sequence) (⌢ (α length))])
               [_ (⌢:bare/weird)]))))

; Forces pattern's syntax-parse error message for compthink users, except for bare id.
(define-macro (handle handler pattern)
  (parser/⊥ [(~and (~is-not id) ~!) ((handler pattern) this-syntax)]))

(lazy-require [(submod "../../english.rkt" syntax) ([non-top message:non-top])])

(define (⌢:non-top this-syntax)
  (define name-from (syntax-parser [(~or* name:id (name:id . _)) #'name]))
  (raise-syntax-error #false (message:non-top (name-from this-syntax)) this-syntax))

(syntax-class ⌢non-top #:attributes {} _ #:when (not (top?)) #:do [(⌢:non-top this-syntax)])
