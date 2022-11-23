#lang snack/pure (provide announce-reading
                          announce-empty-program
                          announce-expanding
                          announce-starting
                          announce-definition)

(require (only-in racket/base when newline)         
         (submod "../../private/print.rkt" custom)
         (submod "../../shared-settings.rkt" feedback)
         (submod "../../configuration.rkt" source-code)
         (only-in "../../words/cs-terms.rkt" term:defined)
         (prefix-in message: "../../words/monologue.rkt")
         snack/definition
         snack/string)

(define (monologue-comment chunks)
  (when (preference:run:monologue)
    (display-styled-chunks (list* (styled-as-comment eol-comment-prefix)
                                  chunks))
    (newline)))

(defines
  [announce-reading       (commenter message:reading)] 
  [announce-empty-program (commenter message:empty-program)] 
  [announce-expanding     (commenter message:expanding)] 
  [announce-starting      (commenter message:starting)]
  #:with [((commenter comment)) (monologue-comment (list (styled-as-comment comment)))])

(defines [(announce-definition id) (monologue-comment (list (styled-as-comment term:defined "")
                                                            (styled-id id)))]
  #:with [(styled-id id) (list (~a id) token:symbol)])
