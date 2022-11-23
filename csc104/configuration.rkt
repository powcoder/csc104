#lang snack/pure

(module source-code racket/base (provide (all-defined-out))
  (define eol-comment-prefix "; ")
  (define expression-comment-prefix "#;"))

; Toolbar, language menu, and language's internal name, for implementation in private/language-ui.rkt.
;  ✪ so should  language-name  be defined elsewhere?
(module language-ui racket/base (require reprovide/reprovide)

  (provide   (prefix-out configuration: (all-defined-out)))
  (reprovide (prefix-in  configuration: (only-in (submod "words/ui.rkt" ui)
                                                 language-name
                                                 language-settings-label)))
  (require (submod "words/ui.rkt" ui))
           
  (require snack/definition)

  (defines
    #;(view:help (icon-view message:help "help" (- 102 50)))
    #;{(render (cut-right (cut-top (align-baselines  (blank 0 14) (text→image  "14" 15)) 3) 1))
       (render (cut-right (cut-top (align-baselines  (blank 0 14) (text→image  "11" 12)) 2) 1))
       (render (cut-right (cut-top (align-baselines  (blank 0 14) (text→image  "8" 10)) 2) 1))}
    #;(view:comment-type (icon-view "Toggle"                  "comment" -5))    
    (view:font-size-8  (icon-view "Font size: "        "font-size-8"  -3))
    (view:font-size-11 (icon-view ""                   "font-size-11" -2))
    (view:font-size-14 (icon-view ""                   "font-size-14" -1))    
    (view:line-comment (icon-view message:line-comment      "comment"  1))
    (view:format       (icon-view message:format           "reindent"  1.5))
    (view:explode      (icon-view message:explode            "explode" 2))
    (view:insert-image (icon-view message:insert-image "insert-image"  3))
    (view:shrink       (icon-view message:shrink             "shrink"  3.5))
    (view:hug          (icon-view message:hug                   "hug"  4))
    (view:definitions  (icon-view message:definitions             "D"  5))
    (view:layout       (icon-view message:layout             "layout"  6))
    (view:record       (icon-view message:record             "record"  6.5))
    (view:interactions (icon-view message:interactions            "I"  7))
    (view:history-up   (icon-view message:history:up       "previous"  8))
    (view:history-down (icon-view message:history:down         "next"  9))

    #:with
    
    (icon-file-collection '("csc104" "media"))
    (icon-file-suffix "-icon.png")
    
    (drracket:check-syntax:button-position 50)
    ; From drracket/private/syncheck/gui.rkt ...
    #;(drracket:module-language-tools:add-opt-out-toolbar-button ⋯ 'drracket:syncheck #:number 50)
    
    ((icon-view label name position)
     (view label (read-bitmap (apply collection-file-path (string-append name icon-file-suffix)
                                     icon-file-collection))
           (+ position drracket:check-syntax:button-position)))

    #:require (only-in snack/button view read-bitmap)))


#;(module lang racket/base

    ; Leaving in for some flexibility:
    [reader-module '(lib "2018-fall-reader.rkt" "csc104")] ; save/load
    
    [module '(lib "csc104/2018-fall.rkt")]

    #;(define/augment (capability-value key)
        (case key
          [(drscheme:help-context-term) "L:csc104/2018-fall"]
          [(tests:test-menu tests:dock-menu) #true]
          [else (inner (drracket:language:get-capability-default key) capability-value key)])))

(module interactions racket/base (provide (prefix-out configuration: (all-defined-out)))

  ; Also for langs.rkt.

  ; compthink-ui/repl-link.rkt gets the language name (not just csc104) via ...
  #;[language (drracket:language-configuration:language-settings-language language-settings)]
  #;[url (send language get-language-url)]
  
  (require snack/string-constants)

  (define language-version "Language Version: 2020-01-12.")
  
  (define course-url "http://www.cs.toronto.edu/~gfb/csc104/2020W")

  (define language-id "csc104:2019-fall") ; unclear on its meaning/use
  ; In “Choose Language” the hierarchy is merged by the language-position names as keys,
  ;  and sorted by the language-numbers (don't know what happens if they're inconsistent).
  ; The one-line-summary is the tooltip that appears when hovering over the innermost.
  (define language-numbers '(-500 -501 1))
  (define language-position
    (list (string-constant teaching-languages) "University of Toronto" "CSC 104"))
  (define one-line-summary "CSC104 Fall 2019"))

(module step-display racket/base (require snack/contract reprovide/reprovide)
  
  (provide (contract-out [a-space-width (→ positive-natural/c)]
                         [sane-minimum-space-width positive-natural/c]
                         [typical-font-aspect-ratio positive-real/c]
                         [maximum-steps      positive-natural/c]
                         [maximum-step-lines positive-natural/c]
                         [maximum-step-width positive-natural/c]))
  (reprovide (prefix-in message: (submod "words/ui.rkt" step-display))
             (only-in (submod "shared-settings.rkt" feedback)
                      preference:display:underline:next
                      preference:display:spacing:compact
                      preference:display:fun:compact
                      preference:interaction:no-wait))

  (require (only-in framework editor:get-current-preferred-font-size)
           (only-in racket/math exact-floor))

  (define-values (typical-font-aspect-ratio sane-minimum-space-width) (values 0.6 8))
  
  (define (a-space-width) (max sane-minimum-space-width
                               (exact-floor (* (editor:get-current-preferred-font-size)
                                               typical-font-aspect-ratio))))
  
  (define-values (maximum-steps maximum-step-lines maximum-step-width)
    (values 50 20 (- 102 (string-length "#;{}")))))
