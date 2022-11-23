#lang snack/pure (provide compthink:time
                          (for-syntax Block) #;compthink:block)

(module runtime racket/base (provide time)
  (require snack/definition snack/port)
  (require (only-in (submod "../../english.rkt" feedback) message:milliseconds)
           (submod "../../private/print.rkt" custom))
  (defines
    [styled-eol-comment-prefix (styled-as-comment eol-comment-prefix)]
    [“:” ":"]
    #:require (submod "../../configuration.rkt" source-code))
  (defines
    [(time thunk expression) ; Run-time support, macro just thunks and quotes.
     (define-values (vs cpu user:ms gc) (time-apply thunk '()))
     (display-styled-chunks (announcement:time user:ms))
     (println expression)
     (apply values vs)]
    #:with
    [(announcement:time user:ms) (list styled-eol-comment-prefix
                                       (styled-as-comment user:ms message:milliseconds “:” ""))]))
(require racket/lazy-require)
(lazy-require  [(submod "." runtime) (time)])

(require
  ; Delegated implementations.
  (only-in "base.rkt" base:#%app base:λ)
  (only-in "../../shared/code.rkt" simply)
  (only-in racket/block [block compthink:block])
  ; Static errors.
  (for-syntax (only-in (submod "syntax-error.rkt" miscellaneous) ⌢:time:arity))
  ; Emfive form Infrastructure.
  (submod "common.rkt" define-syntax-parser/⊥))

; ●  time  ●
(define-syntax-parser/⊥ (compthink:time expression:expr)
  (§∘ (base:#%app time (base:λ () expression) (simply expression)))
  • #:arity ⌢:time:arity)

; ●  block  ●
; All arities and contexts okay. Lacks bare/weird check. Can produce new type  step  needs to know.
(begin-for-syntax (syntax-class Block #:attributes {name es} ; interpreter patterns
                                ((~binding? name #'compthink:block) ~rest es:sequence)))
