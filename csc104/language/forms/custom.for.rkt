#lang racket/base (provide for-loop)

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

(defines [(for-loop name s as λ-body single?)
          (unless (or (list? s) (string? s)) (raise-dynamic-error name for:sequence:value-error))
          (define result (for/fold ([as as]) ([e s])
                           (cond [(not (list? as))
                                  (raise-dynamic-error name for:accumulators:value-error)]
                                 [(not (= (procedure-arity λ-body) (add1 (length as))))
                                  (raise-dynamic-error name for:accumulators:arity-error)]
                                 [else (apply λ-body (if (char? e) (string e) e) as)])))
          (if single? (car result) result)]
  #:with [(needed . ss) (apply ~a␣ "needed" ss)]
  [for:accumulators:arity-error
   (needed "a list with exactly as many elements as the number of accumulators")]
  [for:accumulators:value-error
   (needed "a list of values to match the list of accumulators,"
           "but found"
           "something else")]
  [for:sequence:value-error ; Wouldn't happen for the recursive iterations.
   (needed "a list or text to iterate through,"
           "but found "
           "something else")])
