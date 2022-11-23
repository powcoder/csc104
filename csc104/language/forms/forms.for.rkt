#lang racket/base (provide for-handler for/list-handler)

(require (for-template (except-in racket/base time)
                       (only-in "base.rkt" base:#%app base:list base:for/list base:for*/list base:λ)
                       (only-in "custom.for.rkt" for-loop)
                       (submod "common.rkt" define-syntax-parser/⊥)))

(define-macro (for/list-handler for/list-form-pattern)
  (syntax-parser [(~var || for/list-form-pattern)
                  #:with condition′ (or (α condition) #''#true) #;#'(~? condition #true)
                  (§ (base:for*/list ([e-id′ s]
                                      [e-id (in-value (if (char? e-id′) (string e-id′) e-id′))]
                                      #:when condition′)
                                     body))]))


(define-macro (for-handler for-form-pattern)
  (syntax-parser [(~var || for-form-pattern)
                  #:with [a⒮′ body′ unary?]
                  (if (identifier? (α a-id⒮))
                      #'[(base:#%app base:list a⒮) (base:#%app base:list body) #true]
                      #'[a⒮ body #false])
                  #:attr a⒮- (ξ a⒮)
                  #:with s-   (ξ s)
                  #:with {~and functional- (_ (e-id- a (... ...)) body-)}
                  (expand-for-structure (§ (base:λ parameters body′)))
                  (abstractly (§ (base:#%app for-loop 'name s- a⒮′ functional- unary?))
                              (^for (α e-id-) (^ s-) (α a) (^ a⒮-) (^ body-) (α unary?)))]))

#;(define-macro (for/list-handler for/list-form-pattern)
    (syntax-parser [(~var || for/list-form-pattern)
                    #:with condition′ (or (α condition) #''#true) #;#'(~? condition #true)
                    #:with s- (ξ s)
                    #:with (_ (e-id-) condition- body-)
                    (expand-for-structure (§ (base:λ (e-id) condition′ body)))
                    (abstractly (§ (base:for/list ([e-id- s-] #:when condition-) body-))
                                (^for/list (α e-id-) (^ s-) (^ body-) (^ condition-)))]))
