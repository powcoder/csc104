#lang snack/pure (provide stepper)

(require "annotate.rkt"

         (only-in "interpreter-shared.rkt" reducer/c redex redex? redex-expression)
                  
         (only-in "patterns.rkt" error? error Reduced? Call? Call-name)
         (only-in "patterns.for.rkt" For-step?)
         (only-in "patterns.conditional.rkt" If-step?)
         
         "context.rkt"
         
         (submod "hide.rkt" implementation))

(module decompose racket/base (provide decompose (struct-out decomposition))

  (require "context.rkt" "define.rkt"
           (except-in "interpreter-shared.rkt" reducer)
           "steps.rkt"
           racket/contract snack/contract
           racket/require (multi-in snack (functional list match)))

  #;(provide (contract-out [look (term/c . → . (or/c Context? scope? Hole?))]))

  (define look (∨ id:context define:context or/and:context if:context
                  for:context for/list:context step:context with:context
                  block:context #;arithmetic:context call:context)) ; ✪

  (define-struct/contract decomposition ([context term⊙/c] [redexes (listof redex?)]) #:transparent)

  (define/contract (decompose expression) (term⊙/c . → . decomposition?)
    (match-define (and where (or (scope _ context) context)) (look expression))
    (if (Hole? context)
        (decomposition ⊙ (list (redex (definitions) expression (Hole-reduce-thunk context))))
        (with-locals (if (scope? where) (scope-locals where) '())
          (match-define (list (decomposition contexts redexes) ...)
            (map decompose (Context-unplugged context)))
          (decomposition (apply (Context-builder context) contexts) (apply join redexes))))))

(require 'decompose (only-in "interpreter-shared.rkt" reducer))

(require (only-in racket/sequence sequence-ref)
         (except-in snack/list find-first)
         (except-in (multi-in snack {syntax functional match generator boolean iteration}) §))

#;(contracts [(steps any/c [#:top? boolean? #:newed any/c]) →∗ stream?]
             [(reducible? unreduced?) → boolean?] [(decompose unreduced?) → decomposition?])
#;(define (reducible? e) (hole? (decomposition-context (decompose e))))

(define (mark-next context redexes) #;(transplant e-newed context)
  (plug context (map key:next:set (map redex-expression redexes)))) 

(define-values (value? internal?) (values (make-parameter #false) (make-parameter #false)))

(define (hiding-call? a-call)
  (ormap ∈-hide? (list a-call (and (not (For-step? a-call)) (Call? a-call) (Call-name a-call)))))
(define (skip-one? term) (and (if:hide-introduction?) (If-step? term)))

(define end? (∨ Reduced? error?))

(define (same-call? e e′) (and (Call? e) (Call? e′) (eq? (Call-name e) (Call-name e′))))

(define-values (error-unwrap error-wrapper)
  (values (λ-identity/match [(error _ expression) expression]) 
          (λ-match [(error exn _) (curry error exn)] [_ values])))

(define ((stepper top #:top? [top? #false]) yield)

  (define (blackbox? whole-context r-e reduced-once)
    (and (neither (value?) (error? reduced-once))
         (hiding-call? r-e) (top? . ⇒ . (not (same-call? top r-e)))))
  
  (define (reduced e) (parameterize ([internal? #true] [value? #true]) (steps e)))
  (define (step₁ r′) (sequence-ref (parameterize ([internal? #true] [value? #false])
                                     (generator (stepper r′))) 1))
  
  (define (steps e #:newed [e-newed e])
    (define-macro (yield-e e′) (yield (if (internal?) e e′)))
    (match e
      [(? end?) (if (value?) e (yield-e e #;e-newed))]
      [(app decompose (decomposition context redexes))
       (unless (value?) (yield-e (mark-next context redexes)))
       (define tabulated
         (tabulate new error-unwrap
                   (λ (r+ r) (cond [(blackbox? context (redex-expression r) r+) (reduced r+)]
                                   [(skip-one? r+) (step₁ r+)]
                                   [else r+]))
                   (reducer step₁ reduced)
                   redexes))
       (define error-wrap (error-wrapper (find-first error? (third tabulated))))
       (define in-context (plugger context))
       (steps (error-wrap (in-context (second tabulated)))
              #:newed (error-wrap (in-context (first tabulated))))]))
  
  (steps top))
