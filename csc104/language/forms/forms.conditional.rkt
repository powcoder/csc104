#lang snack/pure (provide compthink:same! compthink:true! compthink:false!
                          emfive:if emfive:else (for-syntax if-shape else-binding)         
                          emfive:and emfive:or  (for-syntax or/and-shape))

; ★ Checks for whether run-time conditions produce boolean. ★
(require (only-in racket/format ~e)
         (only-in snack/string ~a␣ ~a)
         snack/definition
         (only-in (submod "../../english.rkt" error) context-description expect-received)
         (only-in (submod "../../english.rkt" types) type:boolean)
         (only-in "../../words/cs-terms.rkt" term:condition term:value)
         (only-in "../../words/general.rkt" the indefinite ordinal)
         (only-in "../../words/words.rkt" “from”))
(defines [(raise-dynamic-error function/name message)
          (raise (make-exn:fail:contract (context-description (name-else-value function/name)
                                                              message)
                                         (current-continuation-marks)))]
  #:require (only-in "../../shared/code.rkt" name-else-value))
(defines
  [(or/and:checked-boolean name value position)
   (if (boolean? value) value (raise-non-boolean name value #:position (add1 position)))]
  [(if:checked-boolean name value)
   (if (boolean? value) value (raise-non-boolean name value))]
  #:with
  [(message:delimited-value v) (~a␣ "‸" (~e v) "‸")]
  [(raise-non-boolean name #:position [♯position #false] value)
   (raise-dynamic-error name (expect-received
                              (~a␣ (indefinite type:boolean term:value)
                                   “from”
                                   (the (~a (if ♯position (~a (ordinal ♯position) " ") "")
                                            term:condition)))
                              (message:delimited-value value)))])

(require  
  ; Delegated implementations.
  (only-in "base.rkt"
           base:#%app base:quote
           base:begin base:define
           base:cond base:else base:and base:or)
  (only-in "check.rkt" check-expect)
  ; Static errors.
  (for-syntax (only-in (submod "syntax-error.rkt" conditionals)
       
                       ⌢:or/and:arity
                              
                       ⌢:else

                       message:expect-found
                       message:nothing

                       messager:alternative-after-else
                       messager:extra-part/s

                       message:consequent-following
                       message:condition-of-consequent
                       message:condition-and-consequent

                       message:an-else-instead
                       message:another-else-instead

                       message:nothing-after-it
                       message:clause)
              
              (only-in (submod "syntax-error.rkt" miscellaneous)
                       ⌢:same!:arity ⌢:true!:arity ⌢:false!:arity))
  ; Emfive form Infrastructure.
  (submod "common.rkt" define-syntax-parser/⊥)
  ; Generic.
  (for-syntax (multi-in snack {definition conditional boolean})))

; ●  if  else  ●

(begin-for-syntax

  (syntax-class if-binding   #:attributes {} (~binding? #'emfive:if))
  (syntax-class else-binding #:attributes {} (~binding? #'emfive:else))

  (syntax-class if-shape     #:attributes {name ; both
                                           ; here ...
                                           (condition 1) (result 1) else-result
                                           ; interpreter patterns ...
                                           clauses else-name}
                (name:if-binding ~and clauses (:condition-result
                                               ...+
                                               else-name:else-binding else-result:not-else)))
  
  (syntax-class not-else (~and (~is-not else-binding) (~is expr)))
  (syntax-class condition-result #:splice [condition:not-else result:not-else]))

(define-syntax-parser/⊥ emfive:else [_ (⌢:else)])

(define-syntax-parser/⊥ emfive:if
  
  • :if-shape
  #:attr conditions-      (ξ condition)
  #:with [result-    ...] (ξ result)
  #:with else-result-     (ξ else-result)
  #:with [checked-condition- ...]
  (for/list ([condition- (α conditions-)]
             [condition  (α condition)])
    (§∘ condition (base:#%app if:checked-boolean (base:quote name) #,condition-)))
  (abstractly (§ (base:cond [checked-condition- result-] ... [base:else else-result-]))
              (^if (^ conditions-) (^ result-) (^ else-result-)))  

  ; Invalid with an  else  ...
  • ; invalid arity before the  else 
  (_ (~optional (~seq (~any condition-result) condition:not-else)) problem:else-binding (~any expr))
  (current-syntax-error (message:expect-found (if (α condition)
                                                  message:consequent-following
                                                  message:condition-of-consequent)
                                              message:an-else-instead)
                        #'problem)
  • ; valid up to and including the  else 
  (_ (~some condition-result) the-else:else-binding
     ~or* [] [another-else:else-binding (~any expr)] [(~is not-else) extra extras ...])
  (current-syntax-error
   (message:expect-found (messager:alternative-after-else (α extra))
                         (ifs • (α another-else) message:another-else-instead
                              • (α extra) (messager:extra-part/s (add1 (length (α extras))))
                              • else message:nothing-after-it))
   (or (α extra) (α another-else) #'the-else))  

  ; Invalid without an  else :
  • (_ the-clauses:condition-result ... (~optional condition))
  (current-syntax-error
   (message:expect-found (ifs • (α condition) message:consequent-following
                              • (null? (α the-clauses)) message:condition-and-consequent
                              • else message:clause)
                         (if (α condition) message:nothing-after-it message:nothing))
   (α condition)))

; ●  or  and  ●

(begin-for-syntax (syntax-class or/and-shape #:attributes {name ; both
                                                           ; here ...
                                                           (condition 1)
                                                           ; interpreter patterns ...
                                                           neu}
                                ((~or* (~and (~binding? name  #'emfive:or) (~bind (neu #false)))
                                       (~and (~binding? name #'emfive:and) (~bind (neu  #true))))
                                 condition ...)))

(define-syntaxes [emfive:or
                  emfive:and]
  (local [(define (handler delegatee abstraction)            
            (parser/⊥ • :or/and-shape
                      #:when ((length (α condition)) . >= . 1)
                      #:attr conditions- (ξ condition)
                      #:with [checked-condition- ...]
                      (for/list ([(condition- i) (in-indexed (α conditions-))])
                        (§∘ condition- (base:#%app or/and:checked-boolean (base:quote name)
                                                   #,condition- (base:quote #,i))))
                      (abstractly (§∘ (#,delegatee checked-condition- ...))
                                  (abstraction (^ conditions-)))
                      #:arity ⌢:or/and:arity))]
    (values (handler #'base:or  ^or)
            (handler #'base:and ^and))))

; ●  same!  true!  false!  ●

; Last atomic piece of syntax (recursing only into syntax lists, good enough for now).
(define-for-syntax end (syntax-parser [(s.0 ... s) (end (α s))] [_ this-syntax]))

; Whether the last piece of  stx.1  is  on an earlier line than the start of  stx.2 ,
;  when they both have line information.
(define-for-syntax (separate? stx.1 stx.2)
  ((and (syntax-line stx.1)
        (syntax-line stx.2)) . ⇒ . (< (syntax-line (end stx.1))
                                      (syntax-line stx.2))))

; Maybe list of first two syntaxs in  stxs  that aren't separate.
(define-for-syntax (♯joined-pair stxs)
  (for/first ([s.1 stxs] [s.2 (cdr stxs)] #:unless (separate? s.1 s.2)) (list s.1 s.2)))

(define-syntax-parser/⊥ compthink:same! #:top
  • (_ actual:expr expect:expr) (quasisyntax/loc #'expect (check-expect actual expect))
  • (_ . (~and (e1:expr e2:expr es ...) (e:expr ...)))
  (define ♯a-joined-pair (♯joined-pair (α e)))
  (if (not ♯a-joined-pair)
      (§∘ (base:begin  #,(quasisyntax/loc #'e2 (compthink:same! e1 e2))
                       #,(§ (compthink:same! e2 es ...))))
      (current-syntax-error
       (string-append
        "needs its expressions to be on separate lines"
        " when comparing more than two expressions,"
        " but found an expression to compare that does not start on a new line")
       (cadr ♯a-joined-pair)))
  
  #;define-syntax-parser/⊥ ; expands to use of ...
  #;parser/⊥ ; ... which assumes fall-through of a syntax list is due only to arity problem
  ;   ... and then calls the given unary handler with the usage's arity  
  • #:arity ⌢:same!:arity)

(define-syntax-parser/⊥ compthink:true! #:top
  • (_ condition:expr) (§ (check-expect condition #true))
  • #:arity ⌢:true!:arity)

(define-syntax-parser/⊥ compthink:false! #:top
  • (_ condition:expr) (§ (check-expect condition #false))
  • #:arity ⌢:false!:arity)
