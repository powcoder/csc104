#lang snack/pure (require snack/contract)

(provide (contract-out [display/default-color (string? #:newline? boolean? . → . any)]
                       [display-token-line ((listof token/c) #:underlining? boolean? . → . any)]))

(require (submod "../../private/print.rkt" step-display)
         (submod "../interpreter/token.rkt" step-form)
         (multi-in snack {port
                          boolean conditional function}))

(define (display/default-color string #:newline? newline?)
  (print:display/default-color string) (when newline? (newline)))

(define (display-token-line tokens #:underlining? underlining?)
  (define (display-token token)
    (define underline?      (and underlining? (in-next? token)))
    (define underline-bold? (and underline? ((∨ next-start? next-end?) token)))    
    (define content (if (wrapped? token) (wrapped-content token) token))
    (print:display/racket-style
     (if (underline? . ⇒ . (string? content)) content (underline-image content))
     (type-case content
       [delimiter-token? print:token:parenthesis]
       [(∨ boolean-token? number-token? #;box-token? void-token?) print:token:constant]
       [string-token?    print:token:string]
       [image-token?     print:token:image]
       [#:assert identifier-token? print:token:symbol])
     #:delta♯ (cond [underline-bold? print:delta:underline-bold]
                    [underline?      print:delta:underline]
                    [else            #false])))
  (for-each display-token tokens))

(define (underline-image image)
  (local-require (only-in mrlib/image-core make-image image-shape make-bb))
  (define image₁ (above (freeze (add1 (image-width image))
                                (add1 (image-height image))
                                image)
                        (rectangle 0 1 "solid" "black")
                        (rectangle (add1 (image-width image)) 0 "outline" "black")))
  (make-image (image-shape image₁) (make-bb (image-width     image₁)
                                            (image-height    image₁)
                                            (- (image-height image₁) 1))
              #false))
