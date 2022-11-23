#lang racket/base ;  ●  block  step  ●

(require "support.rkt")

(require (for-meta -1
                   (only-in "../forms/forms.binding.rkt" with-shape)
                   (only-in "../forms/forms.misc.rkt" Block)))

(provide-syntax-class
 With-step #:attributes {[definition 1] body builder}
 (ρ :with-shape #:do [(define (namer parts) (slist* #'name parts))
                      (define Ds (syntax-parser [(ds ...) (α ds)]))]
    #:attr builder (syntax-parser∗
                    [([d ...] [d′′ ...])
                     (syntax-parser [d′ (namer (append (Ds #'[d ... d′ d′′ ...]) (list #'body)))])]
                    [(body)
                     (namer (append (Ds #'[definition ...]) (list #'body)))])))

(provide-syntax-class
 Block-step #:attributes {builder es}
 (ρ :Block #:do [(define (namer parts) (slist* (α name) parts))]
    #:attr builder (case-lambda [() (namer (list))] [(e es) (namer (list* e es))])))

(provide-syntax-class
 Step-step #:attributes {e} #:datum-literals (step steps)
 (ρ ((~or* :step :steps) e:expr)))
