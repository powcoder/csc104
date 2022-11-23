#lang racket/base (require snack/contract
                           (submod snack/list core)
                           snack/functional)

(module* step-form #false (provide (struct-out wrapped)
                                   (contract-out [token/c predicate/c]
                                                 [next-start? predicate/c]
                                                 [next-end? predicate/c]
                                                 [in-next? predicate/c]
                                                 [number-token? (any/c . → . any/c)]
                                                 [boolean-token? predicate/c]
                                                 [string-token? predicate/c]
                                                 #;[box-token? predicate/c]
                                                 [void-token? predicate/c]
                                                 [identifier-token? predicate/c]
                                                 [delimiter-token? predicate/c]
                                                 [image-token? predicate/c])
           
                                   freeze rectangle image-width image-height overlay color above))

(module* layout #false (provide (contract-out [token/c predicate/c]
                                              [in-next (any/c . → . in-next?)]
                                              [next-start  ((∨ delimiter-token?
                                                               identifier-token?)
                                                            . → . next-start?)]
                                              [next-end    (delimiter-token? . → . next-end?)])
                                image-width image?))

(require (only-in (combine-in 2htdp/private/image-more mrlib/image-core)
                  freeze image? image-width rectangle image-height overlay color above))

(define-values
  (boolean-token? number-token? string-token? delimiter-token? image-token? #;box-token? void-token?
                  #;space-token?)
  (values (λ• (member? • '("#true" "#false"))) 
          (∧ string? string->number) 
          (∧ string? (λ• (regexp-match-exact? "\".*\"" •))) 
          (λ• (member? • '("(" "[" ")" "]" "{" "}" ",")))
          image?
          #;Box?
          void?
          #;(∨ (∧ string? (λ• (equal? "" (string-trim •))))
               (∧ image?  (λ• (zero? (image-height •)))))))

(define identifier-token?
  (∧ string? (¬ (∨ delimiter-token? boolean-token? string-token? number-token?))))

(define-syntax struct/contract
  (syntax-rules ()
    [(_ (name super) ([id contract] ...) option ...) (struct name super [id ...] option ...)]
    [(_ name ([id contract] ...) option ...) (struct name [id ...] option ...)]))

(struct/contract wrapped ([content any/c]) #:transparent)
(struct/contract (in-next wrapped) () #:transparent)
(struct/contract (next-start  in-next) () #:transparent)
(struct/contract (next-end    in-next) () #:transparent)
#;(require (only-in "../box.rkt" Box?))
(define token/c (or/c string? image? 'next-start 'next-end wrapped?))
