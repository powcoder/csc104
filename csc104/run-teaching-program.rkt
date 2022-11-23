#lang racket/base (require racket/contract/base)

(provide (contract-out [create-empty-module (any/c . -> . (values any/c symbol?))]
                       [expand-teaching-program (input-port?
                                                 (any/c input-port? . -> . any/c)
                                                 any/c
                                                 . -> . (-> any/c))]))

(define module-name '#%compthink)

(require "language/forms/styled-output.rkt"
         (only-in test-engine/racket-tests test))

#;{(require "private/log.rkt")
   (define (show s) (log-compthink-debug "~a : ~a" s (modulo (current-milliseconds) 10000)))}
(define show void)

(define (create-empty-module language-module)
  (show "create-empty-module: start")
  (begin0 (values (datum->syntax #false `(,#'module ,module-name ,language-module)) module-name)
          (show "create-empty-module: end")))
  
(define (expand-teaching-program port reader language-module)
  (define (suck-all-exps port reader)
    (local-require (only-in racket/sequence sequence->list))
    (define (port-reader p) (parameterize ([read-accept-lang #false]) (reader (object-name port) p)))
    (announce-reading)
    (define program (sequence->list (in-port port-reader port)))
    (if (null? program) (announce-empty-program) (announce-expanding))
    program)
  (define state 'init)
  (define saved-exn #f)
  (λ () (case state
          [(init) (set! state 'require)
                  (begin0 (with-handlers ([exn:fail? (λ (x)
                                                       (set! saved-exn x)
                                                       (define-values (mod name)
                                                         (create-empty-module language-module))
                                                       mod)])
                            (define body-expressions (suck-all-exps port reader))
                            (datum->syntax #false
                                           `(,#'module ,module-name ,language-module
                                                       ; avoid macro problems in 'module-begin context
                                                       (#%module-begin . ,body-expressions))
                                           (vector (object-name port) #false #false #false #false)))
                          (show "expand-teaching-program thunk: ready to expand"))]
          [(require) (show "expand-teaching-program thunk: expanded")
                     (set! state 'done-or-exn)
                     (make-dynamic-requirer module-name)]
          [(done-or-exn) (cond [saved-exn (raise saved-exn)]
                               [else #;(announce-start)
                                     eof])])))

(define (make-dynamic-requirer module-name)
  #`(let ([done-already? #f])
      (dynamic-wind void
                    (λ () (dynamic-require ''#,module-name #f))
                    (λ () (unless done-already?
                            (set! done-already? #true)
                            (test)
                            (current-namespace (module->namespace ''#,module-name)))))))
