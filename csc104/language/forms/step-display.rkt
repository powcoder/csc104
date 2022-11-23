#lang snack/pure (require snack/contract)

(provide (contract-out [display-steps! ((listof sequence?) #:wait? boolean?
                                                           #:interactions? boolean?
                                                           #:top? boolean?
                                                           #:language symbol?
                                                           . → . any #;void?)]))

(require (submod "../../configuration.rkt" step-display)
         "print-code.rkt"

         (submod "../interpreter/interpreter.rkt" step-display)
         (only-in "../interpreter/layout.rkt" chunks space-width)

         (multi-in snack {port mutation})
         (multi-in snack {functional string}))

(define (underlining?)     (preference:display:underline:next  #:global? #true))
(define (spacious?)   (not (preference:display:spacing:compact #:global? #true)))
(define (compact-fun?)     (preference:display:fun:compact     #:global? #true))
(define (allow-wait?) (not (preference:interaction:no-wait     #:global? #true)))
  
(define (display-steps! steps
                        #:wait? wait?
                        #:interactions? interactions?
                        #:top? top?
                        #:language language)
  
  (unless interactions? (display/default-color message:header #:newline? #true))
  (when (spacious?) (newline))
      
  (define (display-step! step lead)
    
    ; Get current preferences that could affect intra-step layout.
    (define-values (the-underlining? the-compact-fun?) (values (underlining?) (compact-fun?)))

    (define lead-indent (string-length lead))
    
    ; Whole error: only use the exception. Other error: chunk error-expression. Else chunk expression.
    (unless (whole-error? step)
      (let ([step (if (error? step) (syntax->datum (error-expression step)) step)])
        (parameterize ([space-width (a-space-width)])
          (for* ([(line index) (in-indexed (chunks step
                                                   (- maximum-step-width lead-indent)
                                                   #:compact-fun? the-compact-fun?
                                                   #:language language))]
                 [stop? (in-value (index . >= . maximum-step-lines))] #:final stop?)
            (when (positive? lead-indent)
              (cond [(zero? index) (display/default-color lead #:newline? #false)]
                    [else #;(display/default-color (make-string indent-amount #\space)
                                                   #:newline? #false)
                          (display (make-string lead-indent #\space))]))
            (cond [stop? (display/default-color (message:elided) #:newline? #true)]
                  [else  (display-token-line line #:underlining? the-underlining?) (newline)])))))
    (when (error? step) (define e (error-exn step)) ((error-display-handler) (exn-message e) e)))
    
  (define (maybe-wait)
    (cond [(and (allow-wait?) wait?)
           (flush-output) ; Inside for efficiency of I/O.
           (with-handlers ([exn:fail? (λ₁ 'dont-care)]) (read-line))
           (consume-available!)]
          [(spacious?) (newline)]))

  (define (Δ s.0 s.1) s.1
    #;#;#;
    (local-require (only-in (submod "../interpreter/token.rkt" layout) atom→token)
                   (only-in  "../interpreter/common.rkt"
                             next? markup? markup-expression new?)
                   (only-in  (submod "../interpreter/common.rkt" strings-and-images)
                             image? image-width rectangle above))
    (define-values (s′ _)
      (let Δ ([s.0 s.0] [s.1 s.1])
        (cond [(markup? s.0) (Δ (markup-expression s.0) s.1)]
              [(and (list? s.0) (list? s.1) (= (length s.0) (length s.1))
                    (not (equal? (first s.1) 'if)))
               (for/fold ([prefix '()] [in-shared-prefix? #true]) ([e.0 s.0] [e.1 s.1])
                 (define-values (e.1′ in-shared-prefix?′)
                   (if in-shared-prefix? (Δ e.0 e.1) (values e.1 #false)))
                 (values (append prefix (list e.1′)) in-shared-prefix?′))]
              [(equal? s.0 s.1)
               (values (if (image? s.1)
                           (above (rectangle (image-width s.1) 1 "solid" "gray")
                                  (rectangle 0                 7 "solid" "transparent"))
                           (string->symbol (make-string (string-length (atom→token s.1)) #\.)))
                       #true)]
              [else (values s.1 #false)])))
    s′)
  (local-require racket/stream)
  (local-require (only-in (submod "../interpreter/common.rkt") cleanse))
  (local-require snack/match)
  (local-require racket/control)
  
  (define final-step (void))
  (for*/and ([(top-level n) (in-indexed steps)]
             [first-step (in-value (stream-first top-level))] #:final (namespace? first-step))
    (cond [(namespace? first-step) (unless top? (match (cleanse final-step)
                                                  [`(define . ,_) (void)]
                                                  [(? error?) (abort)]
                                                  [expression (eval expression first-step)]))]
          [else (set! final-step first-step)
                (display-step! final-step (~a message:to-step-lead ␣))
                (flush-output)
                (for/and ([step (stream-rest top-level)]
                          [old-step top-level]
                          #;[n (in-range (sub1 (length (rest top-level))) -1 -1)])
                  (maybe-wait)
                  (cond #;[(n . >= . maximum-steps)
                           (display/default-color (message:elided message:too-many-steps)
                                                  #:newline? #true)
                           #false]
                        [else (display-step! (if #true #;(zero? n) step (Δ old-step step))
                                             (if (spacious?) (~a message:step-lead ␣) ""))
                              (set! final-step step)
                              #true]))
                (maybe-wait)]))
  #;(newline) #| For spacing, and also to produce void. |# )
