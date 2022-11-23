#lang snack/pure (provide compthink-ui@)

(require (only-in "../code-snip.rkt")
         (only-in "../underline-snip.rkt")
         (only-in "../sideline-snip.rkt"))

(require (submod "../configuration.rkt" language-ui)
         (submod "../shared-settings.rkt" feedback)
         "../language/keywords.rkt"
         racket/contract/option "color-lexer/compthink-lexer.rkt"
         "log.rkt")         

(require (multi-in snack {drracket menu button})
         (except-in snack/false λ-case♯)
         (multi-in snack {functional string})
         drracket/tool racket/unit
         (only-in racket/gui style-delta%)
         (only-in framework
                  editor:set-standard-style-list-delta
                  editor:get-standard-style-list))

(module actions racket/base (provide (all-defined-out))

  (require (only-in racket/gui/base editor<%>)
           (only-in framework editor:set-current-preferred-font-size)
           images/flomap
           racket/require
           (multi-in snack {space-out drracket menu keybinding text})
           snack/string-constants
           (multi-in snack {false lambda})
           (submod snack/class use))

  (define (action:font-size size) (λ₁ (editor:set-current-preferred-font-size size)))

  (define (focus-shrink-image focused)
    (let*♯ ([image-snip (♯indicated-image-snip% focused)]
            [bitmap₀ (send image-snip get-bitmap)]) 
           (define fm₀ (bitmap->flomap bitmap₀ #:unscaled? #true))
           (define fm₁ (flomap-scale fm₀ (sqrt 1/2)))
           (define bitmap₁ (flomap->bitmap fm₁ #:backing-scale 1.))
           (send image-snip set-bitmap bitmap₁)))
  
  (define ((focus-keymap-command name) focused)
    ; Could call our custom functions instead.
    (app♯ invoke-function (and (is-a? focused editor<%>) (send focused get-keymap))
          name focused #true))

  (define ((focus-help keymap-thunk) focused)
    (invoke-function (keymap-thunk) ; keymap that adds the function
                     "search-help-desk"
                     focused ; checks it's editor<%> via drracket/drracket/private/rep.rkt get-frame
                     #false)) ; not chaining prevents some exploits?

  (define (action:insert-image this)
    (λ₁ (invoke-menu-callback-by-label/s (send this get-insert-menu) ; menu%
                                         (string-constant insert-image-item))))
  
  (define ((focus-history this up?) focused)
    (when (eq? focused (interactions this))
      (invoke-function (send focused get-keymap) (if up? "put-previous-sexp" "put-next-sexp")
                       focused #true)))

  (define (action:switch-layout this)
    (λ₁ (invoke-menu-callback-by-label/s (send this get-show-menu) ; menu%
                                         (list (string-constant use-horizontal-layout)
                                               (string-constant   use-vertical-layout)))))
      
  (define (action:toggle-record this) (λ₁ (send+ this (get-interactions-text) (toggle-record))))

  (define ((focus-format this) focused)
    (cond [(eq? focused (interactions this)) (send focused tabify-all)]
          [else (define the-definitions (definitions this))
                (send the-definitions tabify-all)
                (fine-format the-definitions)])))
;
(require 'actions (only-in (submod snack/text commands)
                           toggle-comment-type))

(require racket/gui)

(define-unit compthink-ui@ (import drracket:tool^) (export)

  (define frame-mixin
    (mixin (drracket:unit:frame<%>) ()
      (inherit get-menu-bar get-button-panel register-toolbar-buttons get-focus-object)
      (super-new)

      (define (our-lang?) (send+ this (get-definitions-text) (our-lang?)))      
      
      (define/override (on-activate active?)
        (define (log s) (log-compthink-error (~a␣ "language-ui:frame-mixin:on-activate:" s)))
        (let*/else
         ([Language-menu (language-menu (get-menu-bar))] [#:else (log "no Language menu")])
         (define compthink-menu (ensured-item Language-menu 'our-language
                                              configuration:language-settings-label (new menu%)))
         (for ([(preference label) (in-sequences (in-dict  run:labels)
                                                 (in-dict step:labels))])
           (ensured-item compthink-menu (string->symbol label) label
                         (new boolean-preference-item% [preference preference]))))
        (super on-activate active?))

      (define (for-focus handler) (λ₁ (handler (get-focus-object))))
      (define (keymap-command name) (for-focus (focus-keymap-command name)))
      (define button-specs
        (list #;(list configuration:view:help
                      (for-focus (focus-help drracket:rep:get-drs-bindings-keymap)))
              #;(list configuration:view:comment-type (for-focus toggle-comment-type))
              
              (list configuration:view:font-size-8  (action:font-size  8))
              (list configuration:view:font-size-11 (action:font-size 11))
              (list configuration:view:font-size-14 (action:font-size 14))
              (list configuration:view:shrink       (for-focus focus-shrink-image))
              (list configuration:view:line-comment (keymap-command
                                                     "line comment/uncomment around selection"))
              (list configuration:view:explode      (keymap-command "explode"))
              (list configuration:view:format       (for-focus (focus-format this)))
              (list configuration:view:insert-image (action:insert-image this))
              (list configuration:view:hug          (keymap-command "try hug±"))
              (list configuration:view:definitions  (keymap-command "show more of definitions"))
              (list configuration:view:layout       (action:switch-layout this))
              (list configuration:view:record       (action:toggle-record this))
              (list configuration:view:interactions (keymap-command "show more of interactions"))
              (list configuration:view:history-up   (for-focus (focus-history this #true)))
              (list configuration:view:history-down (for-focus (focus-history this #false)))))
      
      (define toolbar (get-button-panel))
      
      (container-edit-sequence
       toolbar
       (define buttons (map (curry apply (button toolbar)) button-specs))       
       ; For sorting by #:numbers, vertical vs horizontal toolbar layout, small vs large, show label.
       (register-toolbar-buttons buttons #:numbers (map (∘ view-position first) button-specs))
       
       (define/public (show/hide-compthink-buttons)
         (container-edit-sequence (get-button-panel)
                                  (for-each (if (our-lang?) show-button hide-button) buttons)))
       (define/augment (on-tab-change old new)
         (show/hide-compthink-buttons)
         (inner (void) on-tab-change old new))
       (show/hide-compthink-buttons))))

  (define keyword-get-token
    (local [(define get-token (waive-option compthink-lexer))]
      (λ (in) (define-values (lexeme type paren start end) (get-token in))
        (values lexeme
                (if (and (eq? type 'symbol) (string? lexeme) (member lexeme emfive-keywords))
                    'keyword type)
                paren start end))))
  
  #| React to Language Change — toolbar and color lexer |#
  (define (definitions-text-mixin %)
    (class % (inherit get-top-level-window get-next-settings) (super-new)
      
      #;(define/override (on-new-image-snip filename kind relative-path? inline?)
          (println (list filename kind relative-path? inline?))
          (super on-new-image-snip filename kind relative-path? inline?))
      
      (define/public (our-lang?)
        (equal? configuration:language-name
                (send (drracket:language-configuration:language-settings-language (get-next-settings))
                      get-language-name)))

      ; Don't reset style after insert (before changing again by coloring), so that a mixture of font
      ;  metrics doesn't cause jitter while typing.
      ; ✪ Consider normalizing newlines though, probably through the lexer, or see about manipulating
      ;  the font metrics.
      #;(define text-mode-mixin
          (mixin (color:text-mode<%> mode:surrogate-text<%>) (-text-mode<%>)
            ⋯ (define/override (on-enable-surrogate text)
                ⋯ (super on-enable-surrogate text) ⋯ (send text set-styles-fixed #t) ⋯) ⋯))
      #;(define other-basics-mixin
          (mixin (editor:basic<%> (class->interface text%)) ()
            ⋯
            (define/augment (on-insert start len)
              (begin-edit-sequence #t #f)
              (inner (void) on-insert start len))
            (define/augment (after-insert start len)
              (set! edition (+ edition 1))
              (when styles-fixed? (change-style (get-fixed-style) start (+ start len) #f))
              (inner (void) after-insert start len)
              (end-edit-sequence)) ⋯))
      (send this set-styles-sticky #true)
      (define/augment (on-insert start len)
        (inner (void) on-insert start len)
        (set! current-styles-fixed (send this get-styles-fixed))
        (send this set-styles-fixed #false))
      (define current-styles-fixed #false)
      (define/augment (after-insert start len)
        (send this set-styles-fixed current-styles-fixed)       
        (inner (void) after-insert start len))
      
      ; Change of settings indirectly calls this somehow : need to check why and whether reliable.
      (define/override (start-colorer token-sym->style- get-token- pairs-)
        (define s (send (editor:get-standard-style-list) find-named-style
                        "framework:syntax-color:scheme:comment"))
        (define sδ (new style-delta%))
        (send s get-delta sδ)
        (send* sδ (set-face #false) (set-family 'swiss) (set-size-mult 1.05))
        (editor:set-standard-style-list-delta "framework:syntax-color:scheme:comment" sδ)
        (super start-colorer token-sym->style- (if (our-lang?) keyword-get-token get-token-) pairs-))

      (define/augment (after-set-next-settings settings)
        
        (define ♯top-level-window (get-top-level-window))
        
        (cond [♯top-level-window
               (container-edit-sequence (send ♯top-level-window get-button-panel)
                                        (send ♯top-level-window show/hide-compthink-buttons)
                                        (inner (void) after-set-next-settings settings)
                                        (send ♯top-level-window sort-toolbar-buttons-panel))]
              [else (inner (void) after-set-next-settings settings)]))))

  (drracket:get/extend:extend-unit-frame frame-mixin)
  (drracket:get/extend:extend-definitions-text definitions-text-mixin))
