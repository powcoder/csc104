#lang racket/base

(require (submod "patterns.rkt" for-fors)
         (only-in (submod "../functions/non-image.rkt" interpreter) §prepender)
         "support.rkt")
  
; Build Some Specific Syntactic Lists and Calls : External Only
(provide §empty-list §prepend) ; stepping  for/list
(define §empty-list (§list '()))
(define (§prepend e l) (build-Call∗ §prepender (list e l)))

(require (for-meta -1 (only-in "../forms/forms.binding.rkt"
                               for-form for/list-form With-clause Each-clause))
         (only-in "../forms/patterns.rkt" List Variable Single))

(provide-syntax-class
 For/List-step
 #:attributes {(parameter 1) s body builder}
 (ρ :for/list-form
    #:with [parameter ...] #'parameters
    #:attr builder (λ (new-s) ((α namer) (map (syntax-parser
                                                [:Each-clause ((α each-builder) new-s)]
                                                [_ this-syntax])
                                              (α clause))))))
(provide-syntax-class
 For-step For-step?
 #:attributes {[parameter 1] es s a⒮ body as builder sequence-stub accumulator-stub builder∗}
 (ρ :for-form
    #:with (~or* (~and _:Variable a-id⒮′:Single)
                 (~and (_:id ...) a-id⒮′:List))
    #'a-id⒮
    #:with (~or* a⒮′:List a⒮′) #'a⒮
    #:attr as (and (or (not (α a-id⒮′.the-length))
                       (equal? (α a-id⒮′.the-length)
                               (α    a⒮′.the-length)))
                   (if (identifier? #'a-id⒮) (list #'a⒮) (α a⒮′.element)))
    #:with (parameter ...) #'parameters
    #:attr builder (λ (new-s new-a⒮ [body′ #'body])
                     ((α namer) (map (syntax-parser [:With-clause ((α with-builder) new-a⒮)]
                                                    [:Each-clause ((α each-builder) new-s)]
                                                    [_ body′])
                                     (α clause))))
    #:attr builder∗
    (λ (es) (apply (α builder) (case (α first-type) [(each) es] [else (reverse es)])))
    #:attr    sequence-stub ((α builder) #'s                          #false #false)
    #:attr accumulator-stub ((α builder) (§list '(#false #false)) #'a⒮  #false)))
