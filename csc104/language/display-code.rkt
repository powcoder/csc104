#lang snack/pure (provide show-anonymized
                          (for-syntax §display-code))

(require (only-in "forms/print-code.rkt" display-token-line)
         (only-in "interpreter/layout.rkt" chunks)

         (submod "../private/print.rkt" custom)
         (submod "../configuration.rkt" source-code)
         
         snack/port

         (multi-in snack {string definition}))

(define maximum-step-width 80)
(define lead-indent (string-length expression-comment-prefix))

(define styled-expression-comment-prefix (styled-as-comment expression-comment-prefix))
(define styled-eol-comment-prefix        (styled-as-comment eol-comment-prefix))
(define (styled-id id) (list (~a id) token:symbol))
(define “:” ":")

(defines [(show-anonymized has? get v)
          (define ♯code (and (has? v) (get v)))
          (cond [♯code (display-code ♯code)]
                [else (display-styled-chunks (list styled-eol-comment-prefix
                                                   (styled-id ‘anonymous’)
                                                   (styled-as-comment "" “:” “no-code”)))
                      (newline)])]
  #:with [‘anonymous’ 'anonymous] [“no-code” "no code available"])


(require (for-syntax racket/base)
         (prefix-in base: (only-in racket/base #%app)))

(define-for-syntax (§display-code §code)
  (quasisyntax/loc §code (base:#%app display-code '#,(syntax->datum §code))))

(define (display-code code)
  (for ([(line index)
         (in-indexed (chunks code (- maximum-step-width lead-indent) #:compact-fun? #true))])
    (cond [(zero? index) (display-styled-chunks (list styled-expression-comment-prefix))]
          [else (display (make-string lead-indent #\space))])
    (display-token-line line #:underlining? #false)
    (newline)))
