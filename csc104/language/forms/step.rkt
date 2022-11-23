#lang snack/pure (provide step R:step python:step
                          steps
                          hide show
                          emfive:if-introduction emfive:if-conditions)

(require
  ; Delegated implementations.
  racket/lazy-require
  (only-in "current-definitions.rkt" get-definitions arities free-id-table-ref)
  (only-in "forms.binding.rkt" close [compthink:with with])
  (only-in "../../shared/code.rkt" syntax→code′)
  syntax/macro-testing
  ; Static errors.
  (for-syntax (only-in (submod "syntax-error.rkt" step)
                       “option-context”
                       “hide-udf” “hide-call-udf” “hide-call-arity”
                       messager:hide:context messager:show:context
                       messager:hide:call:argument:non-literal))
  ; Emfive form Infrastructure.
  (submod "common.rkt" define-syntax-parser/⊥)
  ; Generic.
  (for-syntax (only-in (submod snack/list core) first empty?)
              (only-in snack/function ∨)
              snack/definition))

(begin-for-syntax
  ; Static reflection information from  define .
  (syntax-class udf #:attributes {arity} f:id
                #:attr arity (free-id-table-ref arities (α f) #false)
                #:when (α arity))
  ; Recognizing language literal values.
  (defines [(emfive-literal? expr) ((∨ boolean? string? emfive-number? image? list-literal?)
                                    (syntax->datum expr))]
    #:with
    [emfive-number? real?]
    [image? struct?]
    [(list-literal? v) (and (list? v) (not (empty? v)) (equal? (first v) 'list))])
  ; Keyword binding syntax classes.
  (syntax-class hide-binding #:attributes {} (~binding? #'hide))
  (syntax-class show-binding #:attributes {} (~binding? #'show))
  (syntax-class hide-clause #:attributes {[to-hide 1]} ((~is hide-binding) to-hide:expr ...))
  (syntax-class if-introduction-binding #:attributes {} (~binding? #'emfive:if-introduction))
  (syntax-class if-conditions-binding   #:attributes {} (~binding? #'emfive:if-conditions))
  (syntax-class hide-option (~or* (~is if-introduction-binding)
                                  (~is if-conditions-binding))))


(define-syntax-parser/⊥ emfive:if-introduction [_ (current-syntax-error “option-context”)])
(define-syntax-parser/⊥ emfive:if-conditions   [_ (current-syntax-error “option-context”)])
(define-syntax-parser/⊥ hide [_ (current-syntax-error (messager:hide:context))])
(define-syntax-parser/⊥ show [_ (current-syntax-error (messager:show:context))])
(define-syntax (step         stx) (stepping stx #:interactive? #true))
(define-syntax (R:step       stx) (stepping stx #:interactive? #true #:language 'R))
(define-syntax (python:step  stx) (stepping stx #:interactive? #true #:language 'python))
(define-syntax (steps stx) (stepping stx))

; Delay load of interpreter and display to first invocation of step at runtime.
(module step-display racket/base (provide display-program-steps)
  (require (only-in "step-display.rkt" display-steps!)
           (submod "../interpreter/interpreter.rkt" step-form))
  (define (display-program-steps program get-definitions
                                 #:wait? wait?
                                 #:interactions? interactions?
                                 #:top? top?
                                 #:language language
                                 #:hide hide
                                 #:namespace namespace)
    (display-steps! (parameterize ([definitions (get-definitions)] [current-namespace namespace])
                      (hiding hide (program-steps program)))
                    #:wait? wait?
                    #:interactions? interactions?
                    #:top? top?
                    #:language language)))
;
(lazy-require [(submod "." step-display) (display-program-steps)])

#;(require (only-in racket/base println))
; defined outside a begin-for-syntax so  step  and  steps  see it in their definition context
(define-for-syntax (stepping stx
                             #:interactive? [interactive? #false]
                             #:language [language 'emfive])
  (syntax-parse stx
    [(step-form:id (~optional (~and interactions #:interactions))
                   (~optional the-hide-clause:hide-clause)
                   program ...)
     (when (α the-hide-clause) (check-hide-clause (α the-hide-clause)))
     #;(println #`#,(close #'(program ...)))
     #`(begin
         #;(println #,(close #'(program ...)))
         #;(println (syntax→code′ #,(close #'(program ...))))
         (#;void
          values
          (values #;convert-compile-time-error ; why stop at exactly the first one?
           (cond [#false #,(localize #'(program ...))]
                 [else (display-program-steps
                        #:wait? '#,interactive?
                        #:interactions? '#,(if (α interactions) #true #false)
                        #:top? '#,(and (top?) #true)
                        #:language '#,language
                        #:hide '#,(or (α the-hide-clause.to-hide) '())
                        #:namespace (variable-reference->namespace (#%variable-reference))
                        (syntax→code′ #,(close #'(program ...)))
                        get-definitions)]))))]))

(define-for-syntax (localize stx)
  (syntax-parse stx #:datum-literals {define}
    #;[((~and (:define . _) definition) ...+ . parts) #`(with definition ... #,(localize #'parts))]
    [(expression . parts) #`(cond [#false expression] [else #,(localize #'parts)])]
    [() #'#false]))

(define-for-syntax check-hide-clause
  (parser/⊥ • :hide-clause
            (for ([hidden (α to-hide)])
              (define (error msg) (current-syntax-error msg hidden))
              (syntax-parse hidden
                [(~is hide-option) (void)]
                [(~is udf)         (void)]
                [(:udf arg ...+) #:when (= (length (α arg)) (α arity))
                                 (for ([arg (α arg)] #:unless (emfive-literal? arg))
                                   (define (error msg) (current-syntax-error msg arg))
                                   (error (messager:hide:call:argument:non-literal arg)))]
                [(:udf _ ...) (error “hide-call-arity”)]
                [(_    _ ...) (error “hide-call-udf”)]
                [_            (error “hide-udf”)]))))
