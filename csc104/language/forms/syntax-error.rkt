#lang snack/pure

(require "syntax-error-context.rkt")

(require (rename-in snack/contract [any ⊥])
         snack/syntax (submod snack/syntax syntax-classes)
         snack/definition)

(defines
  [(⌢:call:nullary    f)    (syntax-error′ (message:call:nullary f))]
  [(⌢:call:non:id/fun part) (syntax-error′ (message:call:non:id/fun part))]
  #:with [(syntax-error′ msg)
          (current-syntax-error #:implicit (string->symbol message:term:function-call) msg)]
  #:require (prefix-in message: (only-in (submod "../../english.rkt" syntax)
                                         term:function-call call:nullary call:non:id/fun)))
(provide ⌢:call:nullary ⌢:call:non:id/fun)
(module call racket/base (provide ⌢:call:nullary ⌢:call:non:id/fun)
  (require racket/lazy-require) (lazy-require [(submod "..") (⌢:call:nullary ⌢:call:non:id/fun)]))

; ★ ⌢:bare/weird
(provide (contract-out [⌢:bare/weird (→ ⊥)]))
(splicing-local [(syntax-class bare id:id  #:do [(⌢:bare #'id)])
                 (syntax-class catch-all _ #:do [(⌢:catch-all)])
                 (define (⌢:bare id)   (current-syntax-error (message:bare id)))
                 (define (⌢:catch-all) (current-syntax-error message:catch-all:unexpected-form))
                 (define unreachable 'unreachable)]
  (local-require (prefix-in message: (only-in (submod "../../english.rkt" syntax)
                                              bare catch-all:unexpected-form)))
  (define (⌢:bare/weird) (syntax-parse (context) [(~or* :bare :catch-all) unreachable])))

(define arity/c (natural/c . → . ⊥))
(define (⊥/c domain) (domain . → . ⊥))

(provide (contract-out
          ; We could add a property to note whether it's in function position.
          [⌢:top:unbound (→ ⊥)]
          [⌢:define:redefine (identifier? (♯ (or/c 'lexical non-empty/c)) . → . ⊥)]
          [⌢:define:redefine:user (⊥/c identifier?)]
          [⌢:define:context (→ ⊥)]
          [⌢:define:nothing (→ ⊥)]
          [⌢:define:variable:keyword (⊥/c syntax?)]
          [⌢:define:function:keyword (⊥/c syntax?)]
          [⌢:parameters:keyword      (⊥/c syntax?)]
          [⌢:define:non-binding      (⊥/c syntax?)]
          [⌢:define:function:non-id  (⊥/c syntax?)]
          
          [⌢:parameters:non-id       (⊥/c syntax?)]
          [⌢:define:parameters:function (⊥/c identifier?)]
          [⌢:parameters:duplicate       (⊥/c identifier?)]
          [⌢:parameters:local:duplicate (⊥/c identifier?)]
          [⌢:parameters:none (identifier? boolean? syntax? . → . ⊥)]
          
          [⌢:define:variable:init:arity (natural/c identifier? . → . ⊥)]
          [⌢:function:body:arity        arity/c]
          
          [⌢:with:parts:none       (→ ⊥)]
          [⌢:with:definitions:none (→ ⊥)]
          [⌢:with:non-definition (⊥/c syntax?)]
          [⌢:with:body:arity     arity/c]
          [⌢:with:non-sequence   (⊥/c syntax?)]
          [⌢:with:duplicate      (⊥/c identifier?)]
          
          [⌢:or/and:arity    arity/c]
          [⌢:same!:arity     arity/c]
          [rename ⌢:single-expression ⌢:true!:arity      arity/c]
          [rename ⌢:single-expression ⌢:false!:arity     arity/c]
          [⌢:assuming:arity  arity/c]
          [rename ⌢:single-expression ⌢:time:arity       arity/c]
          [rename ⌢:single-expression ⌢:anonymous:arity  arity/c]
          [rename ⌢:single-expression ⌢:sequential:arity arity/c]
          [⌢:big-bang:arity  arity/c]
          
          [⌢:else (→ ⊥)]))

(require (prefix-in message: (submod "../../english.rkt" syntax)))

(generate
 [[⌢:top:unbound message:top:unbound]
  [⌢:define:redefine:non-user (message:define:redefine:non-user name) detail]
  [⌢:define:redefine:user     (message:define:redefine:user     name) detail]
  ;
  [⌢:define:context message:define:context]
  [⌢:define:nothing message:define:nothing]
  [⌢:define:variable:keyword message:define:keyword          keyword]
  [⌢:define:function:keyword message:define:function:keyword keyword]
  [⌢:parameters:keyword      message:parameters:keyword      keyword]
  [⌢:define:non-binding      (message:define:non-binding      part) as-detail]
  [⌢:define:function:non-id  (message:define:function:non-id  part) as-detail]
  
  [⌢:parameters:non-id (message:parameters:non-id part) as-detail]
  [⌢:define:parameters:function message:define:parameters:function id]
  [⌢:parameters:duplicate       (message:parameters:duplicate       id) as-detail]
  [⌢:parameters:local:duplicate (message:parameters:local:duplicate id) as-detail]
  
  [⌢:function:body:arity (message:function:body:arity amount)]
  
  [⌢:parameters:none (message:parameters:none function anonymous?) header]
  
  [⌢:define:variable:init:arity (message:define:variable:init:arity amount variable)]
  
  [⌢:with:parts:none       message:with:parts:none]
  [⌢:with:definitions:none message:with:definitions:none]
  [⌢:with:non-definition  (message:with:non-definition part)]
  [⌢:with:body:arity      (message:with:body:arity amount)]
  [⌢:with:non-sequence    (message:with:non-sequence part)]
  [⌢:with:duplicate       (message:with:duplicate id) as-detail]
  
  [⌢:or/and:arity      (message:conditions:less-than-one  actual)]
  [⌢:same!:arity       (message:expressions:less-than-two actual)]
  
  [⌢:assuming:arity    (message:expressions:not-two actual)]
  [⌢:single-expression (message:expressions:not-one actual)]
  
  [⌢:big-bang:arity (message:big-bang:arity actual)]  
  
  [⌢:else message:else:context]]
 
 [f:id (~or* message/r:id (message/r:id v₀:id (~optional v₁:id))) (~optional detail:id)]

 (define (f (~? v₀) (~? v₁) (~? [detail v₀] (~? detail)))
   (current-syntax-error (~? (message/r v₀ (~? v₁)) message/r) (~? detail))))

(provide ⌢:fun:bare
         ⌢:fun:header:nullary
         ⌢:fun:header:raw)
(define (⌢:fun:bare) (current-syntax-error "needs to be grouped with one or more parameter names"))
(define (⌢:fun:header:nullary fun)
  (current-syntax-error "needs to be followed by at least one parameter, but found nothing" fun))
(define (⌢:fun:header:raw) (current-syntax-error "this header needs to be grouped with a body"))

(define (⌢:define:redefine id previous-binding)
  (define-values (mp bp) (if (pair? previous-binding)
                             (module-path-index-split (car previous-binding))
                             (values #false #false)))
  ((if mp ⌢:define:redefine:non-user ⌢:define:redefine:user) id id))

(module conditionals racket/base (provide ⌢:or/and:arity ⌢:else ⌢:assuming:arity
                                          (rename-out [expect-found message:expect-found]
                                                      [nothing message:nothing])
                                          message:consequent-following
                                          message:condition-of-consequent
                                          message:condition-and-consequent
                                          message:an-else-instead
                                          messager:alternative-after-else
                                          message:another-else-instead
                                          messager:extra-part/s
                                          message:nothing-after-it
                                          message:clause)
  (require racket/lazy-require)
  (lazy-require [(submod "..") (⌢:or/and:arity ⌢:else ⌢:assuming:arity)])
  (require (only-in (submod "../../english.rkt" syntax)
                    expect-found
                    message:consequent-following
                    message:condition-of-consequent
                    message:condition-and-consequent
                    message:an-else-instead
                    messager:alternative-after-else
                    message:another-else-instead
                    messager:extra-part/s
                    message:nothing-after-it
                    message:clause
                    nothing)))
(module step racket/base (provide (all-defined-out)
                                  messager:hide:context
                                  messager:show:context
                                  messager:hide:non:function/call messager:hide:call:non-id
                                  messager:hide:call:nullary messager:hide:call:argument:non-literal)
  (define “option-context”
    "is out of place here — it is only usable as a ‸step‸ hiding option")
  (define “hide-udf”
    (string-append "needs a function to hide (from a previous definition), or a hiding option," 
                   " but found something else"))
  (define “hide-call-udf”
    (string-append "needs a function call to hide, with a function from a previous definition,"
                   " but found something else"))
  (define “hide-call-arity”
    (string-append "needs the number of arguments in the function call to hide"
                   " to be the number of parameters in the function"))
  (require racket/lazy-require)
  (lazy-require [(submod "../../english.rkt" syntax)
                 (messager:hide:context
                  messager:show:context
                  messager:hide:non:function/call messager:hide:call:non-id
                  messager:hide:call:nullary messager:hide:call:argument:non-literal)]))

(module* binding-forms racket/base (require reprovide/reprovide)

  (reprovide (only-in (submod "..") ⌢:fun:bare ⌢:fun:header:nullary ⌢:fun:header:raw))

  (provide ⌢:each:context)
  (define ⌢:each:context (string-append "not allowed here,"
                                        " because this is not the front of"
                                        " an each clause for"
                                        " an accumulating expression"))
  (reprovide
   (only-in (submod "..")
            ⌢:define:variable:init:arity
            ⌢:define:redefine ⌢:define:context ⌢:define:nothing ⌢:define:non-binding
            ⌢:define:function:non-id
            ⌢:define:parameters:function ⌢:function:body:arity
            ⌢:parameters:none ⌢:parameters:non-id ⌢:parameters:duplicate ⌢:parameters:local:duplicate
            ⌢:with:parts:none ⌢:with:non-sequence
            ⌢:with:definitions:none ⌢:with:duplicate ⌢:with:non-definition
            ⌢:with:body:arity)))

(module big-bang racket/base
  (provide ⌢:big-bang:arity)
  (require racket/lazy-require)
  (lazy-require
   [(submod "..") (⌢:big-bang:arity)]))

(module miscellaneous racket/base (provide message:unusable-character
                                           ⌢:bare/weird ⌢:top:unbound
                                           ⌢:same!:arity ⌢:true!:arity ⌢:false!:arity
                                           ⌢:time:arity ⌢:anonymous:arity ⌢:sequential:arity)
  
  (require snack/definition snack/string)
  (provide (all-defined-out))
  (defines
    ((message:expected #:but-found (but-found #false) . ms)
     (~a (apply ~a␣ "needs" ms) (if but-found (~a␣ "," "but found" but-found) ""))) 
    ((message:expected-but-nothing . ms) (apply message:expected #:but-found nothing ms))
    (message:exactly-one "exactly one")
    #:with (nothing "nothing"))
    
  (defines
    (message:the-function-definition (~a␣ function-definition to-inspect))
    (message:exactly-one-piece (~a␣ message:exactly-one (~a␣ piece-of-code to-inspect)))
    #:with
    (function-definition "a syntactically correct function definition")
    (piece-of-code "piece of code")
    (to-inspect "to inspect"))
  
  (define-values (message:extra-piece
                  message:extra-pieces)
    (values "an extra piece"
            "extra pieces"))

  (defines 
    (message:the-name (~a␣ "the name" function-from-function-definition)) 
    (message:exactly-one-name (~a␣ message:exactly-one "name," function-from-function-definition))
    #:with (function-from-function-definition "of a function from a function definition"))

  (define-values (message:extra-part
                  message:extra-parts)
    (values "an extra part"
            "extra parts"))
  (require racket/lazy-require)  
  (lazy-require [(submod "..") (⌢:bare/weird ⌢:top:unbound
                                             ⌢:same!:arity ⌢:true!:arity ⌢:false!:arity
                                             ⌢:time:arity ⌢:anonymous:arity ⌢:sequential:arity)])
  
  (define (message:unusable-character) "this character is not usable"))

(module patterns racket/base (provide (all-defined-out))
  
  (define an-accumulate-expression "an accumulate expression")
  
  (define a-variable "a variable")
  (define a-variable-or-list-of-variables "a variable or list of variables")
  (define an-element-expression "a body expression")
  (define an-if-clause "an if condition clause")
  (define a-condition-expression "a condition expression")
  (define a-with-clause "a  with  clause")
  (define an-each-clause "an  each  clause")
  (define an-update-expression "a body expression"))
