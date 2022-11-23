#lang snack/pure (provide Map Combine
                          Pipe
                          Select/Sort
                          Repeats Number
                          Compose Partial
                          H²oF)

(require (submod "../functions/non-image.rkt" interpreter)

         (only-in "literal.rkt" Literal §List)
         (only-in "patterns.rkt" Functional)

         "support.rkt"
         
         snack/function)

; Function Constructors
; Integration : steps.rkt, Literal, (submod "non-image.rkt" interpreter), custom.rkt.
; Delayed forms of function calls that would have produced a new function.
(syntax-class Compose #:attributes {§f} (~and (:compose~ :Functional ...) (~is Literal))
              #:attr §f (apply (α value) (α §ⓕ)))
(syntax-class Partial #:attributes {§f} (~and (:partial~ :Functional b)   (~is Literal))
              #:attr §f ((α value) (α §ⓕ) (α b)))

(syntax-class Combine #:attributes {ϕf} :combine~ #:attr ϕf (α value))
(syntax-class Map     #:attributes {ϕf} :map~     #:attr ϕf (∘ §List (α value)))

(syntax-class Pipe    #:attributes {§ⓕ∗} (~and (~is pipe~) :Functional))

; Marshalled
(define-syntax-class Select/Sort #:attributes {unary? §ⓕ}
  (ρ (~and (~is only~) :Functional) #:attr unary? #true)
  (ρ (~and (~is sort~) :Functional) #:attr unary? #false))

(syntax-class Repeats #:attributes {ϕf} :repeats~ #:attr ϕf (∘ §List (α value)))
(syntax-class Number  #:attributes {ϕn}  n:number #:attr ϕn (syntax-e (α n)))

(define-syntax-class H²oF #:attributes {cross? ϕF}
  (ρ :cross~    #:attr ϕF (α value) #:attr cross? #true)
  (ρ :parallel~ #:attr ϕF (α value) #:attr cross? #false))
