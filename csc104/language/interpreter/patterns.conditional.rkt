#lang snack/pure (require "support.rkt") ;  ●  or  and  if   ●

(require (for-meta -1 (only-in "../forms/forms.conditional.rkt" if-shape or/and-shape)))

(provide-syntax-class
 If-step If-step?
 #:attributes {(neutral 1) condition₀ result₀ builder non-boolean-condition-representative}
 (ρ :if-shape
    #:with [[~seq (~and neutral #false) _] ... [~seq condition₀ result₀] delayed ...] (α clauses)
    #:attr builder (λ (new-condition) (§∘ (name #,new-condition result₀ delayed ...)))
    #:attr non-boolean-condition-representative
    (§ (name [~@ neutral '#false] ... condition₀ '#false else-name '#false))))

(provide-syntax-class
 Or/And-step #:attributes {(neutral 1) condition₀ builder non-boolean-condition-representative}
 (ρ :or/and-shape
    #:with [(~same? neutral neu) ... condition₀ delayed ...] (α condition)
    #:attr builder (λ (new-condition) (§∘ (name #,new-condition delayed ...)))
    #:attr non-boolean-condition-representative
    (§ (name neutral ... condition₀))))
