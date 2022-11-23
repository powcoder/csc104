#lang racket/base (require snack/contract)

(module* interpreter    #false (provide hiding))

(module* implementation #false (provide (all-defined-out))

  (require (only-in (submod "../without-gui.rkt" interpreter)
                    hide:if-conditions hide:if-introduction hide:accumulate)
           snack/list)
  
  (define (∈-hide? e) (member? (if (syntax? e) (syntax->datum e) e) (hide)))
  
  (define (for:hide?) (∈-hide? hide:accumulate))
  (define (if:hide-conditions?)   (or (if:hide-introduction?) (∈-hide? hide:if-conditions)))
  (define (if:hide-introduction?) (∈-hide? hide:if-introduction)))

(require snack/syntax)

(define-macro (hiding to-hide:expr an-expr:expr) (parameterize ([hide to-hide]) an-expr))

(define hide (make-parameter '())) #;(parameter/c list?)

#;(define-macro (showing to-show:expr an-expr:expr) (parameterize ([show to-show]) an-expr))
#;(define show (make-parameter '()))
