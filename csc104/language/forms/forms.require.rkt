#lang racket/base (provide compthink:require compthink:only-in compthink:prefix-in)

(require
  ; Emfive form Infrastructure.
  (submod "common.rkt" define-syntax-parser/⊥))

(define-syntax-parser/⊥ compthink:only-in
  [_ (current-syntax-error
      "is out of place here — ‸only-in‸ is only usable inside a ‸require‸ statement")])
(define-syntax-parser/⊥ compthink:prefix-in
  [_ (current-syntax-error
      "is out of place here — ‸prefix-in‸ is only usable inside a ‸require‸ statement")])

(begin-for-syntax ; Keyword binding syntax classes.
  (syntax-class only-binding   #:attributes {} (~binding? #'compthink:only-in))
  (syntax-class prefix-binding #:attributes {} (~binding? #'compthink:prefix-in)))

(require (for-syntax racket/lazy-require))

(module error racket/base (provide check-module parse-only parse-prefix)

  (require (only-in "syntax-error-context.rkt" current-syntax-error)
           (submod "../../english.rkt" require)
           syntax/parse (only-in racket/list rest))
  (require (for-template "base.rkt"))
  
  (define (check-module module-stx)
    ((if (identifier? module-stx) check-module-id-form check-module-string-form) module-stx))
  (define (check-module-id-form module-id-stx)
    (unless (module-path? (syntax-e module-id-stx))
      (current-syntax-error message:invalid-module-id module-id-stx)))
  (define (check-module-string-form module-string-stx)
    (define s (syntax-e module-string-stx))
    (unless (regexp-match #rx#"^[-a-zA-Z0-9_. ]+(/+[-a-zA-Z0-9_. ]+)*$" s)
      (current-syntax-error (cond [(string=? "" s) message:file-string:empty]
                                  [(regexp-match #rx"^/" s) message:file-string:starting-slash]
                                  [(regexp-match #rx"/$" s) message:file-string:ending-slash]
                                  [else message:file-string:disallowed-character])
                            module-string-stx)))

  ; Association list of phase 0 exports of module-path.
  (define (exports module-path)
    ; The second argument should make it so that the module is always declared after this,
    ;  so this is kind of a weird function. Need to do this to be able to use module->exports.
    (module-declared? module-path #t)
    (define-values (vars stxes) (module->exports module-path))
    (define phase-0-vars (assoc 0 vars))
    (define phase-0-stxes (assoc 0 stxes))
    (cond [(and phase-0-vars phase-0-stxes) (append (rest phase-0-vars) (rest phase-0-stxes))]
          [phase-0-vars (rest phase-0-vars)]
          [phase-0-stxes (rest phase-0-stxes)]
          [else (list)]))
  ; Is syntax object id exported by module-path?
  (define (exported? id module-path) (assoc (syntax->datum id) (exports module-path)))
  ; Raise an error if id is not exported by module-path at phase 0.
  (define (check-exported id module-path)
    (unless (exported? id module-path)
      (current-syntax-error (message:variable-not-defined-in-module id module-path) id)))

  ; Delegated parsing of ‘only-in’ and ‘rename-in’. Takes an entire require form as input.
  ; The sub-form should already have been checked to match literal compthink:only-in.
  (define parse-only
    (syntax-parser
      [(_ bare:id . _)   (current-syntax-error message:only-in:bare     #'bare)]
      [(_ (~and (_) it)) (current-syntax-error message:only-in:no-parts #'it)]      
      [(_ (_ module:id))
       (current-syntax-error (message:expect-found message:at-least-one-name-after-library-path
                                                   message:none))]      
      ; Correct syntax
      [(_ (_ m-id:id maybe-spec ...+))
       (check-module-id-form #'m-id)
       (define m (syntax->datum #'m-id))
       (for ([id/pair (attribute maybe-spec)])
         (syntax-parse id/pair
           [ident:id (check-exported #'ident m)]
           [(e-id new-id extra ...)
            (unless (identifier? #'e-id)
              (current-syntax-error (message:expect-found message:require-name #'e-id) #'e-id))
            (check-exported #'e-id m)
            (unless (identifier? #'new-id)
              (current-syntax-error (message:expect-found (message:a-new-name-for #'e-id) #'new-id)
                                    #'new-id))
            (define number-of-extra-parts (length (attribute extra)))
            (unless (zero? number-of-extra-parts)
              (current-syntax-error (message:expect-found message:just-require-rename
                                                          (message:extra-parts number-of-extra-parts))
                                    this-syntax))]
           [_ (current-syntax-error (message:expect-found message:require-name-or-rename id/pair)
                                    id/pair)]))
       (syntax/loc this-syntax (base:require (base:only-in m-id maybe-spec ...)))]
      [(_ (_ non-id . _)) (current-syntax-error (message:expect-found message:a-library-path #'non-id)
                                                #'non-id)]))  
  (define parse-prefix
    (syntax-parser
      [(_ bare:id . _)   (current-syntax-error message:prefix-in:bare     #'bare)]
      [(_ (~and (_) it)) (current-syntax-error message:prefix-in:no-parts #'it)]
      [(_ (_ prefix:id)) (current-syntax-error
                          (message:expect-found message:a-library-path-after-the-prefix
                                                message:none))]
      ; Correct syntax
      [(_ (_ prefix:id m-id:id)) (check-module-id-form #'m-id)
                                 (syntax/loc this-syntax (base:require (base:prefix-in prefix m-id)))]
      [(_ (_ prefix:id non-id))
       (current-syntax-error (message:expect-found message:a-library-path-after-the-prefix #'non-id)
                             #'non-id)]
      ; Too many terms
      [(_ (_ _:id _:id extra ...))
       (current-syntax-error (message:expect-found message:only-prefix-and-path
                                                   (message:extra-parts (length (attribute extra)))))]
      [(_ (_ non-id . _))
       (current-syntax-error (message:expect-found message:a-name-as-a-prefix #'non-id) #'non-id)])))

(begin-for-syntax (lazy-require [(submod "." error) (check-module parse-only parse-prefix)]))

(define-syntax-parser/⊥ compthink:require #:top
  • (_ (~and module (~or* (~is string) (~is identifier)))) (check-module (α module))
  (§ (require module))
  • (~or* (_ (~is only-binding)  . _) ; maybe forgot brackets
          (_ ((~is only-binding) . _)))
  (parse-only this-syntax)
  • (~or* (_ (~is prefix-binding) . _)
          (_ ((~is prefix-binding) . _)))
  (parse-prefix this-syntax)
  • (_) (current-syntax-error "needs a library specification")
  • (_ something-else)
  (current-syntax-error
   (string-append "needs a file name (as text), racket library name,"
                  " prefix-in clause, or only-in clause, but found something else")
   (α something-else))
  • (_ _ extra . _)
  (current-syntax-error "needs a single library specification, but found something extra"
                        (α extra)))
