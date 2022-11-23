#lang snack/pure (provide emfive:fun compthink:define compthink:with
                          (for-syntax close)
                          #%fun
                          (for-syntax fun-shape
                                      define-name-shape binder-body
                                      with-shape)
                          with-local-function) ; — see also •for•

(require
  ; Delegated implementations.
  "base.rkt"
  (only-in (submod "../functions/custom.rkt" forms) wrap-λ define:interaction:redefine-check)
  (only-in "styled-output.rkt" announce-definition)
  (only-in "current-definitions.rkt"
           add-definition! add-anonymized! add-parts!
           arities free-id-table-set!)
  ; Static errors.
  (for-syntax (only-in (submod "syntax-error.rkt" binding-forms)
                       ⌢:fun:bare ⌢:fun:header:nullary ⌢:fun:header:raw)
              (except-in (submod "syntax-error.rkt" binding-forms)
                         ⌢:fun:bare ⌢:fun:header:nullary ⌢:fun:header:raw))
  ; Emfive form infrastructure.
  (submod "common.rkt" define-syntax-parser/⊥)
  ; Specialized form support.
  (for-syntax (only-in syntax/kerncase kernel-form-identifier-list)
              (only-in syntax/context build-expand-context))
  ; Generic. 
  (for-syntax (multi-in snack {definition boolean functional})))

; ●  for  ●
(provide compthink:each compthink:for compthink:for/list)
(begin-for-syntax
  (provide Each-clause With-clause for-form for/list-form)                  
  (require (only-in "patterns.rkt"
                    define-each-clause-form define-with-clause-form
                    define-for-form define-for/list-form)
           (only-in "forms.for.rkt" for-handler for/list-handler))                  
  (define-each-clause-form Each-clause   #'compthink:each)
  (define-with-clause-form With-clause   #'compthink:with)
  (define-for-form         for-form      #'compthink:for      Each-clause With-clause)
  (define-for/list-form    for/list-form #'compthink:for/list Each-clause))
(define-syntaxes (compthink:for compthink:for/list) (values (handle for-handler      for-form)
                                                            (handle for/list-handler for/list-form)))
(define-syntax-parser/⊥ compthink:each [_ (current-syntax-error ⌢:each:context)])

; ● Binding patterns and  close  ●

(begin-for-syntax

  (module raw-patterns racket/base (provide binder+body)

    (require (submod snack/syntax syntax-classes/core))

    ; Always succeeds for  (_ . _) .
    (define-syntax-class binder+body #:attributes {binder name named? named-variable? named-function?
                                                          parameters [parameter 1]
                                                          body-parts body}
      (ρ ((~and binder (~or* (name . parameters) name)) . body-parts)
         #:attr named? (identifier? (α name))
         #:attr named-variable? (identifier? (α binder))
         #:attr named-function? (and (α named?) (α parameters))
         #:with (~maybe (parameter ...)) (α parameters)
         #:with (~maybe (body)) (α body-parts))))
  ;
  (require 'raw-patterns)

  #;((emfive:fun . _) . _)
  ; #%app : name — in  callable  provided to interpreter patterns
  ; close : parameters
  ; interpreter patterns : name parameter body
  (syntax-class fun-shape #:attributes {parameters name (parameter 1) body}
                :binder+body #:when (and (α named-function?) (binding? (α name) #'emfive:fun)))
  (require (only-in "../form-names.rkt" name:fun)
           (only-in "common.rkt" §data))
  (define (§fun that-syntax parameters body-expr)
    (§data that-syntax (list (list* name:fun parameters) body-expr)))

  #;(compthink:define  :id      . _)
  #;(compthink:define (:id . _) . _)
  ; forms : name
  ; close : name parameters
  ; interpreter patterns : all except  parameters
  (define-syntax-class define-name-shape
    #:attributes {name
                  parameters
                  binder named-variable? named-function? (parameter 1) body}
    (ρ ((~binding? #'compthink:define) . :binder+body) #:when (α named?)))

  ; forms : definition
  ; close : definition
  ; interpreter patterns : definition name body
  (syntax-class with-shape #:attributes {[definition 1] name body}
                ((~binding? name #'compthink:with) definition ...+ body))
  
  ; Fully valid separated from larger context.
  ;  • forms.reflection.rkt :  named-function?  name
  ;  •  #%fun  :  parameters  parameter  body
  ;  •  define  : all attributes
  (syntax-class
   binder-body #:attributes {name named-function? parameters [parameter 1] body}
   :binder+body #:when (and (α body) (or (α named-variable?)
                                         (and (α named-function?)
                                              (pair? (α parameter))
                                              (andmap identifier? (α parameter))
                                              (distinct? (cons (α name) (α parameter)))))))

  ; ●  close  ●
  ; Relies on each binding form having :
  ;  • syntax-class with attribute  parameters  with all expressions assumed to be under their scope
  ;  • had its form checked by the time  close  is called
  (syntax-class with:binds #:attributes {parameters}
                with:with-shape
                #:with [definition:define-name-shape ...] (α with.definition)
                #:with parameters (α definition.name))
  (define-syntax-class for-shape:binds #:attributes {parameters} (ρ :for-form))
  (define-syntax-class for/list:binds  #:attributes {parameters} (ρ :for/list-form))
  (define close (syntax-parser [(~or* :fun-shape :define-name-shape :with:binds
                                      :for-shape:binds :for/list:binds
                                      (~is sequence))
                                #:with {id ...} #'{~? parameters {}}
                                #:with {part ...} (stx-map close this-syntax)
                                #' (base:let ([id  #' id] ...)
                                             #` ( #,part ... ))]
                               [id:id #:when (not (lexical? (α id)))
                                      #'   #'   id]
                               [id:id #' #` ' #,id]
                               [datum #'    '   datum])))

; ●  fun  ●
; Exhaustive cases for incorrect  fun  “outer form” i.e. too few parentheses to reach  emfive:#%app .
(define-syntax-parser/⊥ emfive:fun
  • (~is id) (⌢:fun:bare)
  • (fun)    (⌢:fun:header:nullary (α fun))
  • (_ . _)  (⌢:fun:header:raw))
; Remaining forms of  fun , delegated by  emfive:#%app , which includes all correct ones.
; Macro so forms can expand to it without  emfive:#%app  indirection. Shape like  (define (fun ⋯) ⋯) .
(define-parser #%fun   
  • (_ . a-fun:binder-body) #:when (none? lexical? (α a-fun.parameter))                         
  #:with code (close (α a-fun)) 
  #:with {~and anon- (_ (parameter- ...) body-)} (expand-for-structure
                                                  (§ (base:λ a-fun.parameters a-fun.body)))
  (abstractly (§ (base:#%app wrap-λ anon- (base:λ () code))) (^fun (α parameter-) (^ body-)))
  • _ (⌢:function this-syntax))

; ●  define  with  ●

(begin-for-syntax

  (define (definable? id) ((top?) . ⇒ . (not (identifier-binding id))))
  
  (defines [(compthink-defintion-context?) (or (top?)
                                               (in-with?))]
    #:with [(in-with?) (define context (syntax-local-context))
                       (and (pair? context) (expanding-for-with? (car context)))])
  (struct expanding-for-with ())
  (define kernel-forms (kernel-form-identifier-list)))

(define-syntax-parser/⊥ compthink:define
  [(_ . defining:binder-body)
   #:when (and (compthink-defintion-context?) (definable? (α defining.name)))
   #:attr redefine-check (and (interactions-top?) (§∘ (base:#%app define:interaction:redefine-check
                                                                  #'#,this-syntax #'defining.name)))
   #:attr announce (and (definitions-top?) (§ (base:#%app announce-definition 'defining.name)))
   #:attr top-function? (and (top?) (α defining.named-function?))
   #:attr record-arity ; Static info, for  step  to check hiding clauses statically.
   ; Sensible hiding reference : statically-previous user function definition.
   ; Drop recording into expansion after definition, rather than recording within this macro,
   ;  as safest place dynamically/statically to record reference.
   ; See also  http://macrologist.blogspot.com/2013/06/define-vs-attach.html .
   (and (α top-function?) (§∘ (begin-for-syntax
                                (free-id-table-set! arities #'defining.name
                                                    #,(length (α defining.parameter))))))
   #:attr record-definition
   (and (α top-function?) (§∘ (base:#%app add-definition! #'#,this-syntax)))
   #:attr record-anonymized
   (and (α top-function?) (§∘ (base:#%app add-anonymized! defining.name
                                          '#,(syntax->datum (§fun this-syntax
                                                                  (α defining.parameters)
                                                                  (α defining.body))))))
   #:attr record-body
   (and (top?) (§ (base:let-values
                   ({~? [(defining.parameter ...)
                         (base:#%app base:values 'defining.parameter ...)]})
                   (ξ-define defining.name {~? defining.parameters}
                             defining.body))))
   (if (top?)
       (§ (base:begin {~? redefine-check}
                      (base:define-values (defining.name)
                                          (base:begin0 {~? (base:λ defining.parameters defining.body)
                                                           defining.body}
                                                       {~? record-definition}
                                                       ; Get this info from the λ after doing fun?
                                                       ; Also safe outside the define-values since
                                                       ;  partial expansion delays it.
                                                       {~? record-body}))
                      {~? announce}
                      {~? record-arity}
                      {~? record-anonymized}
                      #;{~? record-parts}))
       (§ (base:define defining.name {~? (#%fun (fun . defining.parameters) defining.body)
                                         defining.body})))]
  
  [_ #:when (not (compthink-defintion-context?)) (⌢:define:context)]
  [(_) (⌢:define:nothing)]   
  [:define-name-shape #:when (and #;(definitions-top?) (top?) (not (definable? (α name))))
                      (⌢:define:redefine (α name) (identifier-binding (α name)))]
  [(_ id:id ~rest :sequence) (⌢:define:variable:init:arity (α length) (α id))]
  [(_ (:id . _) . _) (⌢:function this-syntax)]
  [(_ part      . _) (⌢:define:non-binding (α part))])

(define-syntax-parser/⊥ compthink:with
  • :with-shape #:with [definition′:expr ...] (partial-expand-for-error (α definition))
  #:with {~and e- (letrec-values ([(name-:id) body-:expr] ...) with-body-:expr)}
  (expand-for-structure (§ (#%stratified-body definition′ ... body)))
  (abstractly (α e-) (^with (α name-) (^ body-) (^ with-body-)))
  • (_)          (⌢:with:parts:none)
  #;[(_ _)  (⌢:with:definitions:none)] #;[(_ part . _) (⌢:with:non-sequence (α part))])

(define-for-syntax (partial-expand-for-error definitions)
  (define local-context (build-expand-context (expanding-for-with)))
  (define partial-expand (λ• (local-expand • local-context kernel-forms)))
  (for/fold ([seen '()] [partial-expands '()] #:result (reverse partial-expands)) ([d definitions])
    (syntax-parse d [:define-name-shape (define seen′ (cons (α name) seen))
                                        (if (distinct? seen′)
                                            (values seen′ (cons (partial-expand d) partial-expands))
                                            (⌢:with:duplicate (α name)))]
      [_ (⌢:with:non-definition d)])))

(define (with-local-function f parameters body local-body)
  #`(compthink:with (compthink:define (#,f . #,parameters) #,body) #,local-body))

(define-for-syntax (⌢:function stx)
  (define/syntax-parse (_ . (~and :binder+body (~maybe anonymous:fun-shape))) stx)
  (parameterize ([context (or (α anonymous) stx)])
    (cond [(not (α named?)) (⌢:define:function:non-id (α name))]
          [(null? (α parameter)) (⌢:parameters:none (α name) (and (α anonymous) #true) (α binder))]
          [else (for/fold ([seen '()]) ([parameter (α parameter)])
                  (define seen′ (cons parameter seen))
                  ((cond [(not (identifier? parameter)) ⌢:parameters:non-id]
                         [(lexical? parameter) ⌢:parameters:local:duplicate]
                         [(and (not (α anonymous)) (bound-identifier=? (α name) parameter))
                          ⌢:define:parameters:function]
                         [(not (distinct? seen′)) ⌢:parameters:duplicate]
                         [else (λ (_) seen′)])
                   parameter))
                (syntax-parse (α body-parts) [:sequence (⌢:function:body:arity (α length))])])))

(define-syntax-parser ξ-define
  [(_ name:id (~optional (parameter:id ...)) body:expr)
   #:attr body- (ξ body) (§∘ '#,(^definition (α name) (α parameter) (^ body-)))])
