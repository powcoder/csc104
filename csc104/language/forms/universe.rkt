#lang racket/base (provide compthink:big-bang)

(require (submod "common.rkt" define-syntax-parser/⊥)
         (for-syntax (submod "syntax-error.rkt" big-bang)))

(define (view v) v)

(define-syntax compthink:big-bang
  (parser/⊥ #;[_ #'(void)]
            [(_ model:expr (~and clause [kw . args]) ...)
             #:when (not (ormap (♯syntax-parser [((~datum #;~literal to-draw) . _) #true])
                                (stx->list #'(clause ...))))
             #:with import #'(local-require
                              (only-in 2htdp/universe
                                       big-bang
                                       to-draw on-tick
                                       on-key on-mouse key-event? key=? mouse-event? mouse=?
                                       state
                                       stop-when
                                       record?))
             #:with (big-bang to-draw) (datum->syntax #'import '(big-bang to-draw))           
             #:with (kw′ ...) (datum->syntax #'import (syntax->datum #'(kw ...)))
             (quasisyntax/loc this-syntax (let () import
                                            (big-bang model [to-draw view] [kw′ . args] ...)))]
            [(_ part.0 part ...)
             (syntax-local-lift-require '(only 2htdp/universe
                                               to-draw on-tick
                                               on-key on-mouse key-event? key=? mouse-event? mouse=?
                                               state
                                               stop-when
                                               record?)
                                        (syntax-local-lift-require
                                         '(rename 2htdp/universe #%big-bang big-bang)
                                         #'(#%big-bang part.0 part ...)))]
            #:arity ⌢:big-bang:arity
            #;(syntax/loc this-syntax
                (let () (local-require (only-in 2htdp/universe big-bang))
                  big-bang))))
