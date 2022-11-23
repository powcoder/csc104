#lang snack/pure

(provide check-expect)

(require (for-syntax (submod snack/syntax syntax-classes/core))
         (submod "common.rkt" define-syntax-parser/⊥))

(require (only-in test-engine/racket-tests check-expect builder get-test-engine test-format)
         syntax/macro-testing
         snack/control
         (only-in racket/class send))

(define-syntax-parser check-expect
  [(_ test actual) (check-expect-maker this-syntax #'check-values-expected #`test (list #`actual))])

(require (only-in (lib "test-engine/test-info.scm")
                  make-unequal check-fail? check-fail-src unexpected-error)
         (only-in racket/match match-let)
         (only-in "../../continuation-key.rkt" emfive-continuation-marks)
         (only-in setup/collects path->collects-relative)
         (only-in "../../rewrite-error.rkt" exn→rewritten-message))

(define (check-values-expected test actual src test-engine)
  (error-check (lambda (v) (if (number? v) (exact? v) #t)) actual INEXACT-NUMBERS-FMT #t)
  (error-check (lambda (v) (not (procedure? v))) actual FUNCTION-FMT #f)
  (send (send test-engine get-info) add-check)
  (run-and-check (lambda (v1 v2 _) (#;teach-equal? equal? v1 v2))
                 (lambda (src format v1 v2 _) (make-unequal src format v1 v2))
                 test actual #f src test-engine 'check-expect))
(define (run-and-check check maker test expect range src test-engine kind)
  (match-let
      ([(list result result-val exn)
        (with-handlers (#;[exn:fail:wish?
                           (lambda (e)
                             (define name (exn:fail:wish-name e))
                             (define args (exn:fail:wish-args e))
                             (list (unimplemented-wish src (test-format) name args) 'error #f))]
                        [exn:fail?
                         (lambda (e)
                           (define msg (exn→rewritten-message e))
                           (cons (unexpected-error src (test-format) expect msg e)
                                 #;(if (and (pair? kind) (eq? 'check-satisfied (car kind)))
                                       (unsatisfied-error src (test-format) (cadr kind) msg e)
                                       (unexpected-error src (test-format) expect msg e))
                                 (list 'error e)))])
          (define test-val (test))
          (define passes?  (check expect test-val range))
          (cons (or passes? (maker src (test-format) test-val expect range)) (list test-val #f)))])
    (define failed? (check-fail? result))
    (cond [(not failed?) #t]
          [else
           (define c (send test-engine get-info))
           (send c check-failed result (check-fail-src result) exn (and (exn? exn) (exn-srcloc exn)))
           #f])))
(define (exn-srcloc exn)
  (if (exn:srclocs? exn)
      (let ([srclocs ((exn:srclocs-accessor exn) exn)])
        (and (pair? srclocs)
             (car srclocs)))
      (continuation-marks-srcloc exn)))
(define (continuation-marks-srcloc exn)
  (let ([cms (emfive-continuation-marks exn)])
    (cond [(findf (lambda (mark)
                    (and mark
                         (let ([ppath (car mark)])
                           (or (and (path? ppath)
                                    (not (let ([rel (path->collects-relative ppath)])
                                           (and (pair? rel)
                                                (eq? 'collects (car rel))
                                                (or (equal? #"lang" (cadr rel))
                                                    (equal? #"deinprogramm" (cadr rel)))))))
                               (symbol? ppath)))))
                  cms)
           => (lambda (mark) (apply make-srcloc mark))]
          [else #false])))
(define INEXACT-NUMBERS-FMT "same! doesn't want to compare inexact numbers.")
(define FUNCTION-FMT "same! doesn't want to compare functions.")
(define (error-check pred? actual fmt fmt-act?)
  (unless (pred? actual)
    (define msg (if fmt-act? (format fmt actual) fmt))
    (raise (make-exn:fail:contract msg (current-continuation-marks)))))
(define (insert-test test-engine test) (send test-engine add-test test))
(define-for-syntax (check-expect-maker stx checker-proc-stx test-expr embedded-stxes)
  (define bogus-name #`#,(gensym 'test))
  (define src-info #`(list #,@(list #`(quote #,(syntax-source stx))
                                    (syntax-line stx)
                                    (syntax-column stx)
                                    (syntax-position stx)
                                    (syntax-span stx))))
  (define test-expr-checked-for-syntax-error #`(convert-compile-time-error #,test-expr))
  (if (eq? 'module (syntax-local-context))
      #`(define #,bogus-name
          #,#`(let ([test-engine (get-test-engine)])
                (when test-engine
                  (insert-test test-engine
                               (lambda ()
                                 #,(quasisyntax/loc stx
                                     (#,checker-proc-stx
                                      (lambda () #,test-expr-checked-for-syntax-error)
                                      #,@embedded-stxes #,src-info
                                      test-engine)))))))
      #`(let ([test-engine (get-test-engine)])
          (when test-engine
            (insert-test test-engine
                         (lambda ()
                           #,(quasisyntax/loc stx
                               (#,checker-proc-stx
                                (lambda () #,test-expr-checked-for-syntax-error)
                                #,@embedded-stxes #,src-info
                                test-engine))))))))
