#lang snack/pure

(module types racket/base (require "words/meta.rkt"
                                   "words/cs-terms.rkt"
                                   "words/general.rkt"
                                   "words/words.rkt")

  (strings [type:list "list"]
           [type:image "image"])
  
  (provide (prefix-out type: (combine-out boolean function number 
                                          unary-function binary-function 
                                          integer natural-number 
                                          text character)))
  
  (strings [type:non-negative-number     (~a␣ (non- "negative") number)]
           [type:non-zero-number         (~a␣ (non- "zero")     number)]
           [type:positive-number         (~a␣ “positive”        number)]
           [type:positive-natural-number (~a␣ “positive” natural-number)]
           #:with
           [“positive” "positive"]
           [(non- s) (~a "non-" s)])

  (strings [type:angle (~a␣ "angle" "in degrees")]

           [type:font-size (~a␣ "font size"
                                (parenthesize (indefinite number)
                                              "from 1 to 255 inclusive"))]
           
           [type:color "colour"]
           [type:percentage "percentage"]
           [type:color-type
            (~a␣ type:color
                 (parenthesize
                  (indefinite (list-of (plural type:percentage) “of”
                                       (ands (list "red" "green" "blue"
                                                   (~a␣ "optionally" "opacity")))))))]
           [type:list-of-colors (list-of (plural type:color))]

           [type:width "width"] [type:height "height"]
           
           [type:smaller-number (~a␣ type:positive-natural-number "less than 4294967088")]
           
           [type:unicode (~a␣ natural-number (ors (list "at most 55295"
                                                        "between 57344 and 1114111 inclusive")))]
           
           #:with [(list-of . s) (apply ~a␣ type:list “of” s)]))

(module feedback racket/base (require "words/meta.rkt")
  (strings #:prefixed [milliseconds "ms"]))


(module error racket/base (require "words/meta.rkt"
                                   "words/words.rkt"
                                   "words/cs-terms.rkt"
                                   "words/general.rkt")
  (provide (contract-out [context-description ((or/c symbol? string?) string? . → . string?)]
                         [expect-received                    (string? string? . → . string?)]
                         [for-the-nth-argument (exact-positive-integer? . → . string?)]))
  (define (context-description context description) (~a␣ context ":" description))
  (define (expect-received needed received) (comma␣ (~a␣ "needed" needed)
                                                    (~a␣ "but received" received)))
  (define (for-the-nth-argument position) (~a␣ “for” (the (ordinal position) term:argument))))


(module syntax racket/base (require "words/meta.rkt"
                                    "words/cs-terms.rkt"
                                    "words/words.rkt"
                                    "words/general.rkt"
                                    (snacks conditional function match))
  ; • Messager function contracts
  (define arity/c (messager/c natural/c))

  ; • Code components of a message.
  (define (code s) (~a "‸" s "‸"))
  (define (code:indefinite s . rest) (apply indefinite (code s) #:based-on s rest))
  (defines [(code:id id) (code (~id id))] #:with [(~id id) (~s (syntax-e id))])
  (require (only-in "language/form-names.rkt"
                    string-name:define
                    string-name:if string-name:else
                    string-name:step string-name:steps string-name:hide string-name:show))  
  (provide (contract-out [non-top (messager/c identifier?)]
                         [bare    (messager/c identifier?)]
                         [open-parenthesis-before (messager/c symbol?)]))  
  (define (non-top id) (out-of-place (code:id id) (~a␣ “at” (the scope:top-level))))
  (define (bare id) (needs-found (open-parenthesis-before (syntax-e id)) none))
  (define (open-parenthesis-before thing) (indefinite “open-parenthesis” “before” (code thing)))

  ; • General CS Terms
  (define generic:part "part")
  (define-values (generic:grouped generic:clause) (values "grouped" "clause"))
  (define-values (scope:local scope:top-level) (values "local" "top level"))  
  (require (submod ".." types))
  (strings [term:function-call (~a␣ type:function term:call)])
  (defines
    [term:name:variable  (name term:variable)]
    [term:name:function  (name type:function)]
    [term:name:parameter (name term:parameter)]
    #:with [(name thing) (~a␣ thing term:name)])
  (define (name-for . thing) (apply ~a␣ term:name “for” thing))  
  (define a-keyword (indefinite keyword))
    
  ; • Wrong as Part  
  (strings [none "none"] [nothing "nothing"])
  (defines [(wrong part) (define known (type-case (syntax-e part)
                                         [number? type:number]
                                         [string? type:text]
                                         [boolean? type:boolean]
                                         [struct? type:image]
                                         ; consider  list  type here, since it's syntactic for us
                                         #;[list? generic:part]
                                         [else #false]))
                         (if known (indefinite known) “unknown”)]
    #:with [“unknown” "something else"])
  (strings [catch-all:unexpected-form "some grammatical problem"])

  ; • —
  (provide (contract-out [extra-parts (messager/c exact-positive-integer?)]
                         [rename needs-found expect-found
                                 (messager₂/c string? (or/c syntax? string?))]))
  (define (extra-parts n) (n-things n "extra part"))  
  (defines [(needs-found need found:syntax/description)
            (comma␣ (needs need) (~a␣ "but" (found (type-case found:syntax/description
                                                     [syntax? (wrong found:syntax/description)]
                                                     [void? nothing]
                                                     [else found:syntax/description]))))]
    #:with [(found . ss) (apply ~a␣ "found" ss)] [(needs . ss) (apply ~a␣ "needs" ss)])

  (defines [(meaningless-here what there) (— “out-of-place” (~a␣ (code what) “only-usable” there))]
    #:with [(— s.0 s.1) (~a␣ s.0 "—" s.1)]
    [“out-of-place” "is out of place here"] [“only-usable” "is only usable"])
  (define (out-of-place form where-not) (~a␣ "found a use of" form "that is not" where-not))

  ; • Arity in needs-found form.
  (define (arity:needs-found m M kind actual #:pluralize? [pluralize? #true])
    (apply needs-found (arity m M kind actual #:pluralize? pluralize?)))
  
  ; • fun 
  (provide (contract-out [fun:header:bare (messager/c identifier?)]))
  (define (fun:header:bare form)
    (arity:needs-found 2 2 (~a␣ “parentheses” “before” (code:id form)) #:pluralize? #false 1))
  
  ; •  if  else
  
  (strings [else:context
            (meaningless-here string-name:else
                              (~a␣ “before” (the term:alternative)
                                   “in” (code:indefinite string-name:if) term:expression))])
  
  (provide messager:alternative-after-else messager:extra-part/s)
  
  (defines
    [(messager:extra-part/s number-of-extra-parts) ((if (= 1 number-of-extra-parts) indefinite plural)
                                                    (~a␣ “extra” generic:part))]
    [(messager:alternative-after-else extra?) (~a␣ (if extra? “exactly-one” “the”)
                                                   term:alternative “after” (code string-name:else))])
  (strings #:prefixed
           [consequent-following (~a␣ (the term:condition)
                                      “followed-by” “its-consequent”)]
           [condition-of-consequent (~a␣ (indefinite term:condition)
                                         (parenthesize “and” “its-consequent”)
                                         “first”)]
           [condition-and-consequent (ands (list (indefinite term:condition)
                                                 “its-consequent”))]
   
           [an-else-instead      (instead (code:indefinite string-name:else))]
           [another-else-instead (instead “another”  (code string-name:else))]
   
           [nothing-after-it (~a␣ nothing “after” “it”)]
   
           [clause (comma␣ (ands (list (code:indefinite string-name:else) (the term:alternative)))
                           (~a␣ “or” “another” “pair-of” condition-and-consequent))]
   
           #:with
           [(instead . ss) (apply ~a␣ (append ss (list “instead”)))]
           [“followed-by” "to be followed by"]
           [“its-consequent” (~a␣ “its” term:consequent)])

  ; • —
  
  (define (missing  . ss) (needs-found (apply ~a␣ ss) nothing))
  (define (needs-follow-by where what) (arity:needs-found 1 +inf.0 (~a␣ what “after” where) 0))  
  (define ((messager:unlike . ss) part) (needs-found (apply ~a␣ ss) part))

  ; • call
  (provide (contract-out [call:non:id/fun (messager/c (or/c syntax? void?))]
                         [call:nullary    (messager/c (or/c identifier? syntax?))]))    
  (define call:non:id/fun
    (messager:unlike (indefinite type:function “after” (the “open-parenthesis”))))
  (define (call:nullary id/fun) (needs-follow-by (code (syntax->datum id/fun))
                                                 (~a␣ term:argument term:expression)))

  (define (extra for total) (needs-found for (extra-parts (sub1 total)))) 
  
  (match-define (list at-least-one only-one exactly-one just only)
    (local [(define ((prefixer prefix) . s) (apply ~a␣ prefix s))]
      (map prefixer '("at least one" "only one" "exactly one" "just" "only"))))

  
  (define definitions (plural definition))
  (define-values (sequence-of-definitions an-expression)
    (values (~a␣ "sequence of" definitions) (indefinite term:expression)))
  (define for-the-body (~a␣ “for” (the body)))
  (define-values (grouped-definition expression-for-body)
    (values (~a␣ generic:grouped definition) (~a␣ term:expression for-the-body)))

  
  (define-values (was-defined in-language no-redefine) #;"or a required library"
    (values (~a␣ "was" term:defined "previously")
            (~a␣ "is already" term:defined "in our language")
            (~a "and cannot be re-" term:defined)))
  (define ((redefine origin) id) (~a␣ (code:id id) origin no-redefine))

  ; • Simple form arity  
  (provide (contract-out [conditions:less-than-one  arity/c]
                         [conditions:less-than-two  arity/c]
                         [expressions:not-one       arity/c]
                         [expressions:not-two       arity/c]
                         [expressions:less-than-two arity/c]))  
  (define-values (conditions:less-than-one
                  conditions:less-than-two
                  expressions:not-one
                  expressions:not-two
                  expressions:less-than-two)
    (values (curry arity:needs-found 1 +inf.0 term:condition)
            (curry arity:needs-found 2 +inf.0 term:condition)
            (curry arity:needs-found 1      1 term:expression)
            (curry arity:needs-found 2      2 term:expression)
            (curry arity:needs-found 2 +inf.0 term:expression)))

  ; •  define

  ; Context.
  (strings [define:context (meaningless-here string-name:define definition-context)]
           #:with [definition-context (ors (list (~a␣ “at” (the scope:top-level))
                                                 (~a␣ “in” (indefinite “sequence”) “of”
                                                      scope:local definitions)))])
  ; Terms for the parts.
  (define-values (definition-binding
                   the-name-for-the-function
                   the-name-for-a-parameter
                   name-for-the-parameter
                   expression-for-function-body)
    (values (ors (list (indefinite term:name:variable)
                       (~a␣ “parenthesized” term:name:function
                            “with” “its” (plural term:parameter))))
            (the (name-for (the type:function)))
            (the (name-for (indefinite term:parameter)))
            (name-for (the term:parameter))
            (~a␣ term:expression for-the-body “of” (the type:function))))
  (define (expression-after-variable name)
    (~a␣ term:expression “after” (the term:name:variable (code:id name))))

  ; —

  (strings [define:keyword (needs-found definition-binding a-keyword)]
           [define:function:keyword (needs-found the-name-for-the-function a-keyword)])
  
  (provide (contract-out [define:redefine:user     (messager/c identifier?)]
                         [define:redefine:non-user (messager/c identifier?)]
                         [define:non-binding      (messager/c syntax?)]
                         [define:function:non-id  (messager/c syntax?)]))
  
  ; Arity.
  (provide (contract-out [define:variable:init:arity (messager₂/c natural/c identifier?)]))
  (strings [define:nothing (missing definition-binding)])
  ; • variable
  (define (define:variable:init:arity amount variable)
    (if (= amount 0)
        (missing (indefinite (expression-after-variable variable)))
        (extra (exactly-one (expression-after-variable variable)) amount)))
  ; • function
  (define (function:body:arity amount)
    (if (= amount 0)
        (missing (indefinite expression-for-function-body))
        (extra (only-one expression-for-function-body) amount)))  
  
  (define-values (define:redefine:user define:redefine:non-user)
    (values (redefine was-defined) (redefine in-language)))
  (define define:non-binding (messager:unlike definition-binding))
  (define define:function:non-id (messager:unlike the-name-for-the-function))

  (define (different-parameter than) (indefinite name-for-the-parameter "different than" than))
  ; Functions, including anonymous.
  (provide (contract-out
            [function:body:arity  arity/c]
            [parameters:none (messager₂/c identifier? boolean?)]
            [parameters:non-id          (messager/c syntax?)]
            [parameters:duplicate       (messager/c identifier?)]
            [parameters:local:duplicate (messager/c identifier?)]))
  (strings [parameters:keyword (needs-found the-name-for-a-parameter a-keyword)]
           [define:parameters:function (needs-found (different-parameter (the term:name:function))
                                                    (the term:name:function "again"))])
  (define parameters:non-id (messager:unlike the-name-for-a-parameter))
  (define-values (parameters:duplicate parameters:local:duplicate)
    (local [(define ((duplicate for) id) (needs-found for (the "duplicate name" (code:id id))))]
      (values (duplicate (indefinite "unique" name-for-the-parameter))
              (duplicate (different-parameter (the (plural term:parameter)
                                                   "of" (the "enclosing" type:function)))))))
  (define (parameters:none function anonymous?)
    (needs-follow-by (if anonymous? (code:id function) (the term:name:function (code:id function)))
                     term:name:parameter))

  ; • id
  (strings [top:unbound (~a␣ "this" term:variable "is" undefined)])
  
  ; • step
  (provide (prefix-out messager: (contract-out
                                  ; thunking is for step.rkt direct lazy-require
                                  [hide:context messager₀/c] [show:context messager₀/c]
                                  [hide:non:function/call (messager/c (or/c syntax? void?))]
                                  [hide:call:non-id (messager/c syntax?)]
                                  [hide:call:nullary (messager/c identifier?)]
                                  [hide:call:argument:non-literal (messager/c syntax?)])))  
  (defines
    [hide:context (out-of-contexter string-name:hide)]
    [show:context (out-of-contexter string-name:show)]
    #:with [((out-of-contexter form))
            (meaningless-here form (~a␣ “for” “starting” (code:indefinite form generic:clause)
                                        “for” (ors (list (code string-name:step)
                                                         (code string-name:steps)))))])
  (define hide:non:function/call
    (messager:unlike (indefinite (ors (list type:function term:function-call)))))
  (define hide:call:non-id call:non:id/fun)
  (define (hide:call:nullary f-id) (needs-follow-by (code:id f-id) term:argument))
  (defines [hide:call:argument:non-literal (messager:unlike only-literal-function-arguments)]
    #:with [only-literal-function-arguments (only term:literal type:function (plural term:argument))])

  ; big-bang
  (define initial-model "initial model")  
  (provide (contract-out [big-bang:arity arity/c]))  
  (define (big-bang:arity amount) (missing an-expression “for” (the initial-model)))
  
  ; with
  (strings [with:definitions:none (missing (at-least-one grouped-definition))])
  (strings [with:parts:none       (missing (plural grouped-definition))])
  (provide (contract-out [with:body:arity     arity/c]
                         [with:non-sequence   (messager/c syntax?)]
                         [with:non-definition (messager/c syntax?)]
                         [with:duplicate      (messager/c identifier?)]))  
  (define (with:body:arity amount) (if (= amount 0)
                                       (missing (indefinite expression-for-body))
                                       (extra (only-one expression-for-body) amount)))
  (define with:non-sequence (messager:unlike (plural grouped-definition)))  
  (define with:non-definition (messager:unlike (indefinite sequence-of-definitions)))
  (define with:duplicate (redefine (~a␣ was-defined “in” “this” sequence-of-definitions))))


(module rewrite racket/base (require "words/meta.rkt"
                                     "words/general.rkt"
                                     (submod ".." error)
                                     snack/lambda)
  (provide arity λ₁ for-the-nth-argument
           (contract-out [read-syntax:text:image        (any/c         . → . string?)]
                         [read-syntax:illegal-character (any/c string? . → . string?)]))
  (define (read-syntax:text:image        _)
    (context-description "literal text" "cannot contain an image"))
  (define (read-syntax:illegal-character _ c)
    (context-description c "this character is not usable")))


(module stepper racket/base (require "words/meta.rkt" "words/general.rkt")
  (provide (prefix-out message: (contract-out [local:function-escaping (identifier? . → . string?)])))
  (require (submod ".." types) (only-in (submod ".." error) context-description))
  (define (local:function-escaping f)
    (define name:hofs (list "map" "combine" #;"apply" "repeats" "select" #;"sift" "sort"))
    (context-description
     "stepping" (comma␣ (~a␣ type:function (~symbol (syntax-e f))
                             "was defined during stepping and can only be stepped by calling it")
                        (~a␣ "or as the first argument to" (ors name:hofs))))))


(module problems racket/base
  (require "words/meta.rkt"
           "words/cs-terms.rkt"
           (submod ".." types)
           (prefix-in error: (only-in (submod ".." error) expect-received))
           "words/general.rkt")
  (provide code
           (contract-out [predicate-result (symbol? string? string? . → . string?)]
                         [≤-something-of-the (string? string? . → . string?)]
                         [list-length-at-least (exact-positive-integer? . → . string?)]))
  (define (predicate-result for from result)
    (error:expect-received (~a␣ (indefinite type:boolean) "from" from
                                (parenthesize "the" type:function "given to" (~symbol for)))
                           result))
  (define (length-of thing) (~a␣ "length of the" thing))
  (define (list-length-at-least n)
    (if (= n 1) "non-empty list" (~a␣ "list with at least" (n-things n "element"))))
  (strings [length-of-color-list
            (~a␣ "needed" "the" (length-of type:list-of-colors) "to equal the product of"
                 "the" (ands (list (ordinal 2) (ordinal 3)))
                 (~a term:argument (plural-suffix 3))
                 (parenthesize "the" (ands (list type:width type:height))))])  
  (define-values (“=” “>”) (values "equal to" "greater than"))
  (define-values (“≥”) (values (ors (list “>” “=”))))
  (strings [“<” "less than"] [“≤” (ors (list “<” “=”))])  
  (define (≤-something-of-the something the) (~a␣ type:number “≤” "the" something "of the" the))
  (strings [number-out-of-order (≥-the "second argument")]
           #:with [(≥-the the) (~a␣ type:number “≥” "the" the)])
  (define (⧀-length ⧀ type) (~a␣ type:number ⧀ "the" (length-of type)))
  (provide list-index-too-large
           text-index-too-large)
  (define (list-index-too-large ⧀) (⧀-length ⧀ type:list))
  (define (text-index-too-large ⧀) (⧀-length ⧀ type:text))  
  (strings [list-range-index-too-large (⧀-length “≤” type:list)]
           [text-range-index-too-large (⧀-length “≤” type:text)]))


; ● require ●
(module require racket/base (require "words/meta.rkt")  
  (provide (prefix-out message: (combine-out a-library-file-string-cannot
                                             a-library-file-string-can
                                             a-new-name-for
                                             variable-not-defined-in-module
                                             not-library-nor-clause
                                             extra))
           message:expect-found message:extra-parts message:none)  
  (require (prefix-in message: (only-in (submod ".." syntax)
                                        expect-found extra-parts open-parenthesis-before none)))  
  (define (a-library-file-string-cannot restriction) (~a␣ "a library file name cannot" restriction))
  (define (a-library-file-string-can    restriction) (~a␣ "a library file name can"    restriction))  
  (strings #:prefixed
           [be-empty "be empty"]
           [start-with-a-slash "start with a slash"]
           [end-with-a-slash "end with a slash"]
           [a-library-file:contain-only "contain only a-z, A-Z, 0-9, -, _, ., space, and slash"]
           [a-library-path "a library path"]
           [after-the-prefix "after the prefix"]
           [a-name-as-a-prefix "a name as a prefix"]
           [require-name "the name of something to require"]
           [just-require-rename "just the name of something to require and a new name"]
           [require-name-or-rename "a name or an old-name/new-name pair"]
           [only-prefix-and-path "only a prefix and a library path"]
           [a-library-path-or-clause "a library path or clause"]
           [a-library-path-or-clause-after-require "a library path or clause after require"]
           [a-single-library-path-or-clause-after-require
            "a single library path or clause after require"]
           [at-least-one-name-after-library-path "at least one name after the library path"])  
  (define (a-new-name-for id-stx) (~a␣ "a new name for" (syntax->datum id-stx)))
  (define (variable-not-defined-in-module id-stx module-path)
    (~a␣ "the variable or function" (syntax->datum id-stx) "is not defined in" module-path))  
  (strings #:prefixed
           [a-library-path-after-the-prefix (~a␣ a-library-path after-the-prefix)]  
           [only-in:bare
            (message:expect-found (message:open-parenthesis-before 'only-in) message:none)]
           [only-in:no-parts
            (message:expect-found a-library-path message:none)]
           [prefix-in:bare
            (message:expect-found (message:open-parenthesis-before 'prefix-in) message:none)]
           [prefix-in:no-parts
            (message:expect-found a-name-as-a-prefix message:none)]
           [no-parts (message:expect-found a-library-path-or-clause-after-require message:none)]  
           [sub-form-out-of-context "not allowed here, because this is not a require form"]
           [file-string:disallowed-character
            (a-library-file-string-can a-library-file:contain-only)]
           [invalid-module-id "incorrect format for a library path"])  
  (define (not-library-nor-clause part-stx)
    (message:expect-found a-library-path-or-clause part-stx))
  (define (extra extra-stx)
    (message:expect-found a-single-library-path-or-clause-after-require
                          (message:extra-parts (length (syntax->list extra-stx)))))  
  (strings #:prefixed
           [file-string:empty (a-library-file-string-cannot be-empty)]
           [file-string:starting-slash (a-library-file-string-cannot start-with-a-slash)]
           [file-string:ending-slash (a-library-file-string-cannot end-with-a-slash)]))
