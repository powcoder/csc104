#lang racket/base (provide Variable Single List
                           define-for-form define-for/list-form
                           define-with-clause-form
                           define-each-clause-form
                           §for/list)

(require (only-in "common.rkt" sequence) (submod "syntax-error.rkt" patterns)
         snack/syntax (submod snack/syntax syntax-classes/core))

(require "forms.for.rkt")

; ★ Simple Generic
(define-syntax-class Variable #:attributes {}
  #:description a-variable (ρ (~describe #:opaque #false (~is id))))
(define-syntax-class Single #:attributes {elements} (ρ _ #:with elements (list this-syntax)))

; ★  List
(require (for-meta -1 (only-in "forms.list.rkt" emfive:list)))
(define-syntax-class List #:attributes {elements ; here
                                        ; interpreter patterns and/or interpreter : elements, and ...
                                        (element 1) the-length
                                        #;builder}
  #:description "a list"
  (ρ ((~binding? _ #'emfive:list) ~rest (~and elements:sequence (element ...)))
     #:attr the-length (length (α element))
     #;{#:attr builder (syntax-parser [(element ...) #'(name element ...)])}))

; ★ Internal Clause
(require (for-meta -1 (only-in "forms.conditional.rkt" emfive:if)))
(define-syntax-class If-clause #:no-delimit-cut
  ; here
  #:attributes {condition} ; otherwise _ seems to be an attribute
  #:description an-if-clause
  (ρ ((~binding? _ #'emfive:if) ~! (~describe #:opaque a-condition-expression condition:expr))))

; ★ with clause, for for/list, each clause

(define-macro (define-each-clause-form each-clause-form-name compthink-each)
  (define-syntax-class each-clause-form-name #:no-delimit-cut
    #:attributes {e-id s expression type ; here
                       ; interpreter patterns ...
                       each-builder}
    #:description an-each-clause
    (ρ ((~binding? each-name compthink-each) ~! (~describe #:opaque a-variable e-id:Variable) s:expr)
       #:attr each-builder (syntax-parser [s #'(each-name e-id s)])
       #:with expression #'s #:attr type 'each)))

(define-macro (define-with-clause-form with-clause-form-name compthink-with)
  (define-syntax-class with-clause-form-name #:no-delimit-cut
    #:attributes {a-id⒮ a⒮ a-id⒮.elements expression type ; here
                        ; interpreter patterns ...
                        with-builder}
    #:description a-with-clause
    (ρ ((~binding? with-name compthink-with)
        ~! (~describe #:opaque a-variable-or-list-of-variables
                      (~or* (~and _:Variable a-id⒮:Single)
                            (~and (_:id (... ...)) a-id⒮:List)))
        (~or* a⒮:List a⒮))
       #:attr with-builder (syntax-parser [a⒮ #'(with-name a-id⒮ a⒮)])
       #:with expression #'a⒮ #:attr type 'with)))

(define-macro (define-for-form for-form-name compthink-for each-clause-form with-clause-form)
  (define-syntax-class for-form-name #:description an-accumulate-expression
    ; close : parameters
    ; for-handler : name parameters body s a-id⒮ a⒮ e-id
    ; interpreter patterns : a-id⒮ a⒮ parameters s body first-type clause namer es
    #:attributes {parameters
                  name s a-id⒮ a⒮ body
                  namer e-id (clause 1) es first-type}
    (ρ ((~binding? name compthink-for)
        (~and (~seq (~or* (~var clause with-clause-form)
                          (~var clause each-clause-form)
                          clause)
                    (... ...))
              (~seq (~alt (~once (~var with with-clause-form) #:name a-with-clause)
                          (~once (~var each each-clause-form) #:name an-each-clause)
                          (~once (~describe an-update-expression body:expr)
                                 #:name an-update-expression))
                    (... ...))))
       #:with (a-id⒮ a⒮) #'(with.a-id⒮ with.a⒮)
       #:with (e-id s) #'(each.e-id each.s)
       #:with parameters #'(e-id . with.a-id⒮.elements)
       #:attr namer (λ (parts) (slist* #'name parts))
       #:do [(define (non-#false l) (filter values l))]
       #:attr first-type (car (non-#false (α clause.type)))
       #:with es              (non-#false (α clause.expression)))))


(require (only-in "../form-names.rkt" name:for/list name:each)
         (only-in "common.rkt" §data))
(define an-expression-for-the-sequence "an expression for the sequence")
(define-macro (define-for/list-form for/list-form-name compthink-for/list each-clause-form)
  (define-syntax-class for/list-form-name
    #:description #false ; note: form name is always prefixed to the error
    ; close : parameters
    ; for/list-handler : e-id s condition body
    ; interpreter patterns : clause parameters namer s body
    #:attributes {parameters
                  e-id s condition body
                  #;name namer (clause 1)}
    (ρ ((~binding? name compthink-for/list)
        (~describe an-element-expression body:expr)
        (~or* (~datum :) (~datum for))
        ~!
        (~describe #:opaque a-variable e-id:Variable)
        (~or* (~datum ∈) (~datum in))
        (~describe #:opaque "an expression for the sequence" s:expr)
        (~optional (~seq (~binding? _ #'emfive:if) ~!
                         (~describe #:opaque a-condition-expression condition:expr))))
       #:with parameters #'(e-id)
       #:with (clause (... ...)) '()
       #:attr namer void)
    (ρ ((~binding? name compthink-for/list)
        (~and (~seq clause (... ...))
              (~seq (~alt (~once (~var || each-clause-form) #:name an-each-clause)
                          (~optional :If-clause #:name an-if-clause)
                          (~once (~describe an-element-expression body:expr)
                                 #:name an-element-expression))
                    (... ...))))
       #:with parameters #'(e-id)
       #:attr namer (λ (parts) (slist* #'name parts)))))
(define (§for/list that-syntax body-expr variable-id list-expr)
  (§data that-syntax #;(list name:for/list body-expr (list name:each variable-id list-expr))
         (list name:for/list body-expr ': variable-id '∈ list-expr)))
