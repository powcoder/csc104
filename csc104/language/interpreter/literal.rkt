#lang snack/pure

; Recognize literal.
;   — and for  fun  : abstract its parts
; Build list literal.

(provide Literal Fun Else
         §List literal←value)

; To determine if an identifier is bound to a function,
;  where the function is not being managed by the stepper's environment
;  (need to describe details of “managed”).
(require (only-in "eval.rkt" Eval))

; Non-list non-function values
; Void and boxes are experimental and in-progress.
; Should boxes be treated similarly to compound function expressions?
(require (only-in (submod "common.rkt" kinds) image?)
         #;(only-in "../box.rkt" Box?))

(require (only-in "../forms/patterns.rkt" List)
         (only-in (submod "../without-gui.rkt" interpreter) §list-constructor)
         (only-in (submod "../functions/non-image.rkt" interpreter) functional)
         (for-template (only-in "../forms/forms.binding.rkt"     fun-shape)
                       (only-in "../forms/forms.conditional.rkt" else-binding)))

(require "support.rkt"
         snack/function snack/conditional)

(define-syntax-class Literal #:attributes {}
  (ρ (~true (datum? (syntax-e this-syntax)) _))
  (ρ (~is Else))
  (ρ (~and (~is id) (~true (procedure? (Eval this-syntax)))))
  (ρ (~is Fun))
  (ρ (~and :List (~parse ((~is Literal) ...) (α elements))))
  (ρ ((~is functional) (~is Literal) ...)))

(define datum? (∨ boolean? string? real? image? void? #;Box?))
(syntax-class Fun #:attributes {name (parameter 1) body} :fun-shape)
(syntax-class Else #:attributes {} (~is else-binding))

; Syntactic list from elements, using canonically named emfive binding.
; • marshalling lists from racket, map and repeats whose step produces a list, fold-like forms
(define (§List es) (slist* §list-constructor es))

; Only for reducers.
(define (literal←value value) #;((not/c ≈term/c) . → . literal?)
  (type-case value
    [list? (§List (map literal←value value))]
    [procedure? (object-name value)]
    [else value]))
