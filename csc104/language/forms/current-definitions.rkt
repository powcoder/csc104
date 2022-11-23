#lang snack/pure (provide (all-defined-out)
                          (for-syntax arities free-id-table-ref free-id-table-set!))

(require (for-syntax (only-in syntax/id-table
                              make-free-id-table
                              free-id-table-ref free-id-table-set!))
         (multi-in snack {mutation
                          syntax function}))

(define-for-syntax arities (make-free-id-table))

(define-values (get-definitions add-definition!)
  (let ([current-definitions (box '())])
    (values (λ () (reverse (unbox current-definitions)))
            (λ (definition) (set-box! current-definitions
                                      (cons definition (unbox current-definitions)))))))

;  eq? -based table producing  #false  for non-existent
;  • key equality appropriate for symbolic names and function pointers
;  • non-existent value appropriate for 
(define-macro (table getter:id has?:id setter:id)
  (define-values (getter has? setter)
    (let ([the-table (make-hasheq)])
      (values (curry hash-ref the-table)
              (curry hash-has-key? the-table)
              (curry hash-set! the-table)))))

(table get-anonymized has-anonymized? add-anonymized!)
(table get-parts      has-parts?      add-parts!)
