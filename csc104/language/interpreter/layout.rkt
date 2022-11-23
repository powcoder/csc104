#lang snack/pure (require snack/contract
                          snack/syntax
                          (multi-in snack {boolean match functional definition})
                          (except-in snack/false λ-case♯)
                          (submod snack/list core))
(provide (contract-out [space-width (parameter/c positive-natural/c)]
                       [chunks (any/c natural/c
                                      #:compact-fun? boolean?
                                      #:language symbol?
                                      . → . chunked/c)]))

(require "layout-support/layout-rules.rkt")

(define chunked/c (listof (listof token/c)))

(define (chunks definition/expression maximum-width #:compact-fun? compact-fun? #:language language)
  (app♯ (∘ simplify-chunks marked→chunked)
        ((case language
           [(R) R:markup]
           [(python) python:markup]
           [else emfive:markup])
         (→ast definition/expression)
         maximum-width
         #:compact-fun? compact-fun?)))

#;(contracts [force? (parameter/c boolean?)]
             [(chunks any/c natural/c) → (♯ chunked/c)]
             [(markup any/c natural/c) → (♯ layout/c)]
             [(marked→chunked layout/c [(listof token/c)]) →∗ chunked/c]
             [(number-of-lines layout/c) → natural/c])

(module simplify racket/base (provide simplify-chunks)
  
  (require (submod "token.rkt" layout)
           snack/functional
           snack/list
           snack/match)

  (define (simplify-chunks chunks)
    (define non-trivials (map (λ• (filter-not (λ-♯match `"") •)) chunks))
    (define (nextify tokens in-next?)
      (match tokens
        [`(next-start ,t next-end . ,rest)
         (define-values (tokens′ in-next?′) (nextify rest #false))
         (values (list* (in-next t) tokens′) in-next?′)]
        [`(next-start ,t          . ,rest)
         (define-values (tokens′ in-next?′) (nextify rest #true))
         (values (list* (next-start t)  tokens′) in-next?′)]
        [`(,t   next-end          . ,rest)
         (define-values (tokens′ in-next?′) (nextify rest #false))
         (values (list* (next-end t)    tokens′) in-next?′)]
        [`(,t . ,rest)
         (define-values (tokens′ in-next?′) (nextify rest in-next?))
         (values (list* (if in-next? (in-next t) t) tokens′) in-next?′)]
        [`() (values `() in-next?)]))
    (for/fold ([result '()] [in-next? #false] #:result result)
              ([n-t non-trivials])
      (define-values (n-t′ in-next′) (nextify n-t in-next?))
      (values (append result (list n-t′)) in-next′))))

(require 'simplify)
