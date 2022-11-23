#lang racket/base (provide new key:next:set key:next:cleanse key:next→next)

(require (only-in "patterns.rkt" error?)
         (only-in "common.rkt" next new)
         #;(only-in "common.rkt" #;new? #;markup-expression #;cleanse)
         (submod snack/syntax syntax-classes/core))

#;(define (transplant expression+ context)
    (define-values (un-newed re-newer) (if (new? expression+)
                                           (values (markup-expression expression+) new)
                                           (values expression+ (λ• •))))
    (re-newer (type-case context
                [list? (map transplant un-newed context)]
                [hole? (next un-newed)]
                [else un-newed])))

(define key:next 'compthink:next)
(define (key:next? stx) (syntax-property stx key:next))
(define (key:next:set stx) (syntax-property stx key:next #true))
(define (key:next:unset stx) (syntax-property-remove stx key:next))
(define (key:next:cleanse s)
  (if (error? s) s (let key:next:cleanse ([s s]) (if (key:next? s) 
                                                     (key:next:cleanse (key:next:unset s))
                                                     (syntax-parse s
                                                       [(s ...) (syn (map key:next:cleanse (α s)))]
                                                       [_ s])))))
(define (key:next→next s)
  (if (error? s) s (let key:next→next ([s s]) (if (key:next? s)
                                                  (next (key:next→next (key:next:unset s)))
                                                  (syntax-parse s
                                                    [(s ...) (map key:next→next (α s))]
                                                    [_ (syntax-e s)])))))
