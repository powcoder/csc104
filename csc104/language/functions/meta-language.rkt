#lang snack/pure

; Function (type) Signature
;   Constrained by class  contracting .
;   The  ~or*  cases are handled by  define/provide , where class  signature handles the variadic
;     and non-variadic cases.
;   Arity checking is automatic, except for any non-homogeneous part of a variadic rest parameter.
;   The contract following the header is required for variadic, which must be binary to receive the
;    function id and a list of *all* the arguments.

(module definer ; Mainly about arranging the contract checking.

  ; ToDo : review the division of contract and name handling between this and (submod ".." help)

  ; Optimizations :
  ;  • contract  any/C  (todo: recognize by binding) is elided
  ;  • contracts are callable, but locating the predicate field has been optimized
  ;  • fixed argument checks are inserted conjoined, checked without reporting machinery,
  ;     and only if the conjunction is false is the full machinery used

  ; Potential Optimizations :
  ;  • move more code out of body (e.g. the full-machinery check is a function of only the arguments)
  ;     for more racket inlining opportunities
  ;  • check fixed arguments of variadic separately
  ;  • recognize contract statically and insert its predicate rather than extracting the predicate
  ;     — inline predicate body
  ;  • use racket's chaperones/impersonators

  racket/base (provide definer)
  
  (require (only-in "C.rkt" /C-predicate)
           (prefix-in base: (only-in racket/base
                                     begin define provide unless and quote
                                     [#%plain-app #%app]
                                     list list*)))
  (require racket/lazy-require)
  (lazy-require [(submod "error.rkt" public) (check-arguments-contracts)])
  
  (require snack/syntax (for-syntax (submod snack/syntax syntax-classes)
                                    (submod snack/syntax syntax-classes/core)))

  (define-macro (definer (f:id . formals) dynamic-name checking:expr ...+ body:expr)

    #:with the-checks (syntax-parse #'[(f:id . formals) checking ...]
                      
                        [[:variadic-formals check-all:id]
                       
                         #'[(base:#%app check-all dynamic-name (base:#%app base:list* . arguments))]]
                      
                        [[(:id arg:id ...+) [~bracketed C:expr ...+] (~optional check-all:id)]
                       
                         ; small sanity-check
                         #:when (= (length (α arg)) (length (α C)))
                       
                         #:with [check ...]
                         (for/list ([an-arg (α arg)] [a-C (α C)] #:unless (eq? (syntax-e a-C) 'any/C))
                           #`(base:#%app (base:#%app /C-predicate #,a-C) #,an-arg))
                       
                         #:attr ♯checks (and (not (null? (α check))) #'[check ...])
                       
                         #'[(~? (base:unless (base:and . ♯checks)
                                             (base:#%app check-arguments-contracts dynamic-name
                                                         (base:#%app base:list C   ...)
                                                         (base:#%app base:list arg ...))))
                            (~? (base:#%app check-all dynamic-name arg ...))]])
    
    (base:begin (base:define (f . formals) [~@ . the-checks] body)
                (base:provide f))))

(module help racket/base (provide contracting alias/rename/from define/provide)
  
  (require snack/syntax
           (submod snack/syntax syntax-classes)
           (submod snack/syntax syntax-classes/core)
           syntax/parse/experimental/template)

  ; Playing around with handling of optional parts of syntax.
  (define-template-metafunction proxy (syntax-parser [(_ _ e) (α e)]))
  (define-pattern ~option [(_ instance:id k:keyword) #'(~optional (~and instance k))])
  
  ; Higher-order contracting.
  (require (for-template (only-in (submod "error.rkt" non-image)
                                  unary-predicate/C binary-predicate/C)
                         (only-in (submod "error.rkt" higher-order)
                                  checked-unary-predicate checked-binary-predicate)))
  (define-syntax-class wrap #:literals {unary-predicate/C binary-predicate/C}
    (ρ :unary-predicate/C  #:attr wrapper #'checked-unary-predicate)
    (ρ :binary-predicate/C #:attr wrapper #'checked-binary-predicate))

  (require (for-template (submod ".." definer)
                         (prefix-in base: (only-in racket/base
                                                   local-require only-in
                                                   [#%plain-app #%app] quote begin
                                                   apply))))

  (syntax-class contracting #:splice #:attributes {f}
                [(f:id ~or* [_:id v:id ...] _:id [_ . _:id] [_ _ . _:id]) (~optional dependency:id)]
                #:do [(define ♯fixed-non-unary (and (α v) (length (α v))))
                      (define dependency? (α dependency))]
                #:when (if ♯fixed-non-unary
                           (if (= 0 ♯fixed-non-unary) (not dependency?) (<= 1 ♯fixed-non-unary 3))
                           dependency?))

  (syntax-class
   (alias/rename/from f there custom′)
   #:splice [(~optional [~bracketed #:aka alias:id ...+])
             (~optional (~seq #:renames original:id) #:defaults ([original f]))
             (~optional (~or* (~and #:custom (~bind [library (datum->syntax there custom′)]))
                              (~seq #:from library)))
             (~option ♯self-option #:self)]
   #:attr data
   (list f (α original) (temporary #'renamed-in) (or (α alias) '{}) #'{(~? ♯self-option)}))

  (define-splicing-syntax-class signature #:attributes {contract/s app formals arguments checker}
    (ρ (~seq header:variadic-formals contract/s:id) #:with {{~optional checker}} '{}
       #:attr app #'base:apply
       #:with formals #'header.formals #:with arguments #'header.arguments)
    (ρ (~seq (_:id contract:id ...+) (~optional checker:id))
       #:attr app #false #:with contract/s #'[contract ...]
       #:with formals (generate-temporaries #'contract/s) #:with arguments #'formals))

  (define (define/provide whole-specification library default binding-data)
    (syntax-parse binding-data
      [[f original imported-as {alias ...} {(~optional ♯self-option)}]
       #:with import
       #`(base:local-require (base:only-in #,(or library default) [original imported-as]))
       (syntax-parse whole-specification
         [(_ :signature . _)
          #:with [name         ...] #'[f alias ...]
          #:with [dynamic-name ...] #'[(base:quote name) ...]
          (syntax-parse #'[contract/s arguments]
            [(~or* [[:wrap _ ...] [p a ...]] _)
             #'(base:begin (definer (name . formals) dynamic-name contract/s (~? checker)
                             (base:begin
                              import
                              (base:#%app (~? app) imported-as
                                          (~? (proxy ♯self-option dynamic-name))
                                          (~? [~@ (base:#%app wrapper dynamic-name p) a ...]
                                              [~@ . arguments]))))...)])])])))

(module client racket/base (provide (all-from-out (submod "error.rkt" public))
                                    (for-syntax (all-from-out (submod ".." help)))
                                    (all-from-out snack/syntax))
  (require (submod "error.rkt" public) (for-syntax (submod ".." help)) snack/syntax))

(module non-image racket/base (provide (for-syntax custom)
                                       (all-from-out (submod ".." client)
                                                     (submod "error.rkt" non-image)))
  (require (submod ".." client) (submod "error.rkt" non-image))
  (define-for-syntax custom "custom.rkt"))

(module image racket/base (provide (for-syntax custom)
                                   (all-from-out (submod "error.rkt" image)
                                                 (submod ".." client)))
  (require (submod ".." client) (submod "error.rkt" image))
  (define-for-syntax custom "image-implementation.rkt"))
