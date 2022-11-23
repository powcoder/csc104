#lang racket/base

(module rewrite racket/base (provide (all-defined-out))
  
  (require (submod "../../english.rkt" error)
           "../../words/general.rkt"
           "../../words/cs-terms.rkt"
           racket/require (multi-in snack {string match functional}))

  (define (contract-to-desc ctc)
    (with-handlers ([exn:fail:read? (λ₁ ctc)])
      (let loop ([s (read (open-input-string ctc))])
        (match s
          [#false "false"]
          [(? symbol?) #:when (regexp-match? #rx"[?]$" (symbol->string s))
                       (define str (symbol->string s))
                       (indefinite (substring str 0 (sub1 (string-length str))))]
          [`(or/c  . ,s) (ors (map loop s))]
          [`(and/c . ,s) (indefinite (~a␣ "value that is" (ands (map loop s))))]
          [`(not/c   ,s) (indefinite (~a␣ "value that is not" (loop s)))]
          [`(>/c 0) (indefinite "positive number")]
          [`(</c 0) (indefinite "negative number")]
          [`(>=/c 0) (indefinite "non-negative number")]
          [_ (~a ctc)]))))

  (define (contract-error-message _ ctc given pos)
    (format "needed ~a~a~a~a, but received ~a"
            (contract-to-desc ctc) (if pos " as " "") (or pos "") (if pos " argument" "") given))
  (define →image   (λ₁ (regexp-replace-quote   "an image")))
  (define →number  (λ₁ (regexp-replace-quote   "a number")))
  (define →integer (λ₁ (regexp-replace-quote "an integer")))
  (define number (∘ cardinal string->number))
  (define (expects₂ _₀ _₁ two) (format "needed a ~a" two))
  (define suppress (λ_ ""))

  (define (arity-message name min-arity max-arity found)
    (~a␣ (if name (~a␣ name ":") "")
         (apply expect-received (arity (string->number min-arity) (string->number max-arity)
                                       term:argument (string->number found))))))

(module internal racket/base (provide (all-defined-out) /C-name)

  (require "C.rkt"
           (prefix-in message: (only-in (submod "../../english.rkt" error)
                                        context-description expect-received for-the-nth-argument))
           "../../words/general.rkt"
           "../../words/cs-terms.rkt"
           (only-in racket/format ~e) snack/string)

  (define (check-argument-contract f-id C argument [position #false])
    (unless ((/C-predicate C) argument) (check-argument f-id #false (/C-name C) argument position)))

  (define (check-argument f-id argument-ok? expected-message actual [position #false])
    (unless argument-ok? (err f-id (message:expect-received
                                    (~a␣ (indefinite expected-message)
                                         (if position
                                             (~a␣ (message:for-the-nth-argument position))
                                             ""))
                                    (~e actual)))))

  (define (wrong-arity f-id m M arguments)
    (err f-id (apply message:expect-received (arity m M term:argument (length arguments)))))
  
  (define (err f-id message) (raise (make-exn:fail:contract (message:context-description f-id message)
                                                            (current-continuation-marks)))))

(module public racket/base (provide (all-defined-out) (all-from-out "contracts.rkt"))

  (require (submod ".." internal) "contracts.rkt")

  (define ((dependent-argument-checker . dependencies) f-id . arguments)
    (for ([dependency dependencies])
      (define n (car  dependency))
      (define C (cadr dependency))
      (check-argument f-id (apply C arguments) (/C-name C) (list-ref arguments (sub1 n)) n)))

  (define ((binary-checker binary-contract) f-id a.1 a.2)
    (check-argument f-id (binary-contract a.1 a.2) (/C-name binary-contract) a.2 2))

  (define ((contract-checker C) f-id . arguments) (unless (apply C arguments) (err f-id (/C-name C))))

  (define ((arity-arguments-contract-checker min-arity . Cs) f-id arguments)
    (define max-arity (length Cs))
    (if (> (length arguments) max-arity)
        (wrong-arity f-id min-arity max-arity arguments)
        (check-arguments-contracts f-id Cs arguments)))

  (define ((arguments-homogeneous-contract-checker C #:offset [offset 0]) f-id arguments)
    (unless (andmap C arguments) (for ([(a i) (in-indexed arguments)] #:unless (C a))
                                   (check-argument-contract f-id C a (+ 1 offset i)))))

  (define ((checker-fixed-homogeneous fixed-contracts homogeneous-contract) f-id arguments)
    (define fixed-arity (length fixed-contracts))
    (local-require (only-in racket/list split-at))
    (define-values (fixed homogeneous) (split-at arguments fixed-arity))
    (check-arguments-contracts f-id fixed-contracts fixed)
    ((arguments-homogeneous-contract-checker homogeneous-contract #:offset fixed-arity)
     f-id homogeneous))

  #;(symbol? (listof C?) list? . → . any/⊥)
  (define (check-arguments-contracts f-id Cs arguments)
    (for ([a (in-list arguments)] [C (in-list Cs)] [i (in-naturals 1)])
      (check-argument-contract f-id C a i))))

(module non-image racket/base (provide (all-from-out (submod "contracts.rkt" non-image)))  
  (require (submod "contracts.rkt" non-image)))
(module     image racket/base (provide (all-from-out (submod "contracts.rkt"     image)))
  (require (submod "contracts.rkt"     image)))

(module higher-order racket/base (provide checked-unary-predicate checked-binary-predicate)  
  (require (only-in (submod ".." internal) err) (only-in racket/format ~e)
           (prefix-in message: (only-in (submod "../../english.rkt" problems) code predicate-result)))
  (define ((checked-unary-predicate  for-name p?) e)
    (let ([pe (p? e)])     (if (boolean? pe) pe (predicate-error for-name p? pe))))
  (define ((checked-binary-predicate for-name p?) e₀ e₁)
    (let ([pe (p? e₀ e₁)]) (if (boolean? pe) pe (predicate-error for-name p? pe))))
  (define (the-name f)
    (define name (object-name f))
    (message:code (cond [(syntax? name) (syntax->datum name)] [name name] [else f])))
  (define (predicate-error for-name from result)
    (err for-name (message:predicate-result for-name (the-name from) (~e result)))))
