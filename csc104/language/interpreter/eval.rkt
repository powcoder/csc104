#lang snack/pure ; Connection to emfive runtime via namespace reflection.

(provide Eval (struct-out error) whole-error? message→error)

(define ∅ #'⌢)

(require snack/definition)

(define (Eval r) (with-handlers ([exn:fail? whole-error]) (eval r)))
  
(struct error (exn expression) #:transparent) #;{exn:fail? any/c}

(define (whole-error exn) (error exn ∅))
(define (whole-error? step) (and (error? step) (equal? (error-expression step) ∅)))
(define (message→error message) (whole-error (exn:fail message (current-continuation-marks))))
