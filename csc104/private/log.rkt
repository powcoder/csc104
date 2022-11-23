#lang racket/base (require snack/contract)

(provide compthink-logger with-compthink-log-fail)

(require snack/false syntax/parse/define (for-syntax racket/base))

(define-logger compthink)

(define-simple-macro (with-compthink-log-fail body:expr ...+)
  (with-handlers ([exn:fail? (λ (e) (log-compthink-error (exn-message e)))]) body ...))

(define-simple-macro (define-Δ-loggers logger:id ...+ #;(~multiple logger:id))
  #:with (Δ-logger ...) (generate-temporaries #'(logger ...))
  #:when (andmap identifier-binding (syntax->list #'(logger ...)))
  (begin (provide (rename-out [Δ-logger logger] ...))
         (... (define-simple-macro (Δ-logger string/format-string v ...)
                (let ()
                  (let*♯ ([Δ (♯Δ-string)]) (logger Δ))
                  (logger string/format-string v ...))))
         ...))

(define-Δ-loggers
  log-compthink-fatal
  log-compthink-error
  log-compthink-warning
  log-compthink-info
  log-compthink-debug)

#;(contracts [last-log-time (parameter/c exact-integer?)]
             [(♯Δ-string) → (♯ string?)])

(define last-log-time (make-parameter (current-seconds)))

(define (♯Δ-string)
  (define new-time (current-seconds))
  (define Δ (min 3 (inexact->exact (floor (log (max 1 (- new-time (last-log-time))) 2)))))
  (last-log-time new-time)
  (and (positive? Δ) (make-string Δ #\⌛)))
