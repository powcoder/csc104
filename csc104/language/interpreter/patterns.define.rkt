#lang racket/base

(require "support.rkt")

(require (for-meta -1 (only-in "../forms/forms.binding.rkt" define-name-shape)))

(provide Define? Define Define-name Define-value
         Define-Function? Define-Function
         Define-Variable? Define-Variable)

(define Define?          (parses? (~is Define)))
(define Define-Function? (parses? (~is Define-Function)))
(define Define-Variable? (parses? (~is Define-Variable)))
(define Define-value (syntax-parser [(~or* :Define-Variable :Define-Function) (α value)]))
(define Define-name  (syntax-parser [:Define (α name)]))

(define-syntax-class Define
  #:attributes {binder name named-variable? named-function? [parameter 1] body}
  (ρ :define-name-shape))

(define-syntax-class Define-Function #:attributes {name value body [parameter 1]}
  (ρ :Define #:when (α named-function?) #:attr value (α name)))

(define-syntax-class Define-Variable #:attributes {value body}
  (ρ :Define #:when (α named-variable?) #:attr value (α body)))

(provide-syntax-class
 Define-step #:attributes {builder body}
 (ρ (~and :Define (define . _)) #:attr builder (λ (body) (slist (α define) (α binder) body))))
