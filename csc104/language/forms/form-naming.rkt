#lang snack/pure (provide define-provider map-macro
                          as-keywords as-identifier)

(require snack/syntax (for-syntax racket/syntax))

(begin-for-syntax (struct provision (provider name module-name in-name) #:transparent
                    #:property prop:procedure (struct-field-index provider)))

(define-syntax-parser define-provider
  [(_ as:id reference-as:id module implementation-name:id)
   #:do [(define (make-name prefix) (format-id this-syntax "~a:~a" prefix #'reference-as))]
   #:with provider-as (make-name "provide")
   #:with module-name (if (string? (syntax-e #'module))
                          (format "forms/~a" (syntax-e #'module))
                          #'module)
   #:with symbol-as   (make-name "name")
   #:with string-as   (make-name "string-name")
   #:with string-name (symbol->string (syntax-e #'as))
   #'(begin (begin (define symbol-as 'as)          (provide symbol-as))
            (begin (define string-as 'string-name) (provide string-as))
            (define-syntax provider-as
              (provision (syntax-parser
                           [(_) #:with name (datum->syntax this-syntax 'implementation-name)
                                #'(reprovide (only-in module-name [name as]))])
                         'as 'module-name 'implementation-name))
            (provide provider-as))])

(define-syntax-parser as-keywords
  [(_ provision:id ...+)
   #:with names (map provision-name (map syntax-local-value (attribute provision)))
   #'(begin (provision) ...
            (module keywords racket/base (provide emfive-keywords) (define emfive-keywords 'names)))])

(define-syntax-parser as-identifier
  [(_ as:id provision:id)
   #:do [(define the-provision (syntax-local-value (attribute provision)))]
   #:with module-name (provision-module-name the-provision)
   #:with in-name     (provision-in-name     the-provision)
   #:with name        (provision-name        the-provision)
   #'(begin (require (only-in module-name [in-name name]))
            (define as #'name)
            (provide as))])
