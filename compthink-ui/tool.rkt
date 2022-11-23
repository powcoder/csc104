#lang snack/pure (provide tool@) (require drracket/tool racket/unit)

(require "configuration.rkt"
         "repl-link.rkt"
         "menus-keys.rkt"
         (submod snack/text commands)

         ; Some Mutation.
         (only-in racket/base set!)
         snack/control

         (only-in snack/text edit-sequence racket:text<%> get-∥s)
         (only-in snack/style underlining)

         (multi-in snack (boolean class))
         (submod snack/class implement))

(require framework racket/gui)

(define-unit tool@ (import drracket:tool^) (export drracket:tool-exports^)

  (define (phase1) (void))

  (define (phase2)
    
    (drracket:get/extend:extend-unit-frame (mixin (drracket:unit:frame<%>) () (super-new)
                                             (drracket-keybindings)
                                             (drracket-menu-items this)))

    (drracket:get/extend:extend-definitions-text
     racket:text<%>:comment-out-selection-mixin)
         
    (define racket:text<%>:tabify-up-to-return-mixin
      (mixin (racket:text<%>) () (super-new)
        (inherit begin-edit-sequence end-edit-sequence
                 get-start-position tabify-selection)
        (define/override (insert-return)
          (begin-edit-sequence #true #false) ; undoable / interrupt-streak
          (tabify-selection 0 (get-start-position))
          (super insert-return)
          (end-edit-sequence))))
    (drracket:get/extend:extend-definitions-text racket:text<%>:tabify-up-to-return-mixin)
    #;(define/public (tabify-all) (tabify-selection 0 (last-position)))
    ; Seems unused in framework and drracket, except by do-return in keymap.
    #;(define/public (insert-return)
        (begin-edit-sequence #t #f)
        (define end-of-whitespace (get-start-position))
        (define start-cutoff
          (paragraph-start-position (position-paragraph end-of-whitespace)))
        (define start-of-whitespace
          (let loop ([pos end-of-whitespace])
            (if (and (> pos start-cutoff)
                     (char-whitespace? (get-character (sub1 pos))))
                (loop (sub1 pos))
                pos)))
        (delete start-of-whitespace end-of-whitespace)
        (insert #\newline)
        (when (and (tabify-on-return?)
                   (tabify (get-start-position)))
          (set-position
           (let loop ([new-pos (get-start-position)])
             (if (let ([next (get-character new-pos)])
                   (and (char-whitespace? next)
                        (not (char=? next #\newline))))
                 (loop (add1 new-pos))
                 new-pos))))
        (end-edit-sequence))
    #;(add-edit-function "do-return"  
                         (λ (x) (send x insert-return)))
    ; private/racket.rkt:   (add-edit-function "do-return"  
    ; private/racket.rkt:   (send keymap map-function "return" "do-return")
    ; private/racket.rkt:   (send keymap map-function "s:return" "do-return")
    ; private/racket.rkt:   (send keymap map-function "s:c:return" "do-return")
    ; private/racket.rkt:   (send keymap map-function "a:return" "do-return")
    ; private/racket.rkt:   (send keymap map-function "s:a:return" "do-return")
    ; private/racket.rkt:   (send keymap map-function "c:a:return" "do-return")
    ; private/racket.rkt:   (send keymap map-function "c:s:a:return" "do-return")
    ; private/racket.rkt:   (send keymap map-function "c:return" "do-return")
    ; private/racket.rkt:   (send keymap map-function "d:return" "do-return")
    ; private/racket.rkt:   (map-meta "return" "do-return")
    ; private/racket.rkt:   (map-meta "s:return" "do-return")
    ; private/racket.rkt:   (map-meta "s:c:return" "do-return")
    ; private/racket.rkt:   (map-meta "a:return" "do-return")
    ; private/racket.rkt:   (map-meta "s:a:return" "do-return")
    ; private/racket.rkt:   (map-meta "c:a:return" "do-return")
    ; private/racket.rkt:   (map-meta "c:s:a:return" "do-return")
    ; private/racket.rkt:   (map-meta "c:return" "do-return")
    
    #| ★ Interactions ★ |#
    
    (drracket:get/extend:extend-interactions-text
     (interactions-link-mixin drracket:rep:text<%>
                              drracket:language-configuration:language-settings-language))

    (drracket:get/extend:extend-interactions-text
     (mixin (drracket:rep:text<%>) () (super-new)
     
       (repl-keybindings this)

       (inherit get-start-position get-end-position last-position)
     
       ; Automatic Formatting
       (inherit tabify-selection get-unread-start-point)
       (define (tabify-repl) (tabify-selection (get-unread-start-point) (last-position)))
       (define/override (do-paste start time) (super do-paste start time) (tabify-repl))
       (define/override (on-local-char event)
         (when (eqv? (send event get-key-code) #\return) (tabify-repl))
         (super on-local-char event))
     
       ; Prompt Text
       (define/override (get-prompt) configuration:prompt-text)

       ; ★ Interactions Recording ★
       
       (inherit get-user-thread get-top-level-window
                begin-edit-sequence end-edit-sequence
                change-style)
     
       (define (♯definitions)
         (define ♯tlw (get-top-level-window))
         (and (is-a? ♯tlw drracket:unit:frame<%>) (send ♯tlw get-definitions-text)))
     
       (define recording 'no)
       ;
       (define (style-prompt)
         (begin-edit-sequence #false #true)
         (define the-prompt-start (prompt-start))
         (when (positive? the-prompt-start)
           (change-style (underlining (case recording [(no) #false] [else #true]))
                         the-prompt-start (+ the-prompt-start 1)))
         (end-edit-sequence))
       ; ﹔
       (define/public (toggle-record)
         (begin-edit-sequence #false #true)
         (define the-prompt-start (prompt-start))
         (define allowing-edits? (send this get-allow-edits))
         (send this set-allow-edits #true)
         (send this delete the-prompt-start prompt-position)
         (set! recording (case recording [(no) 'comment] [else 'no]))
         (send this insert (get-prompt) the-prompt-start)
         (send this set-allow-edits allowing-edits?)
         (style-prompt)
         (end-edit-sequence))
       ;
       (define/augment (on-submit)
         (send (send this get-context) clear-execution-state)
         (when (and (not (equal? recording 'no))
                    (get-user-thread) (thread-running? (get-user-thread)))
           (let*♯
            ([defs (♯definitions)])
            (define defs-last-position₀ (send defs last-position))
            (define-values (defs-start defs-end) (get-∥s defs))
            (when (send defs can-insert?
                        defs-last-position₀ (+ 1 (- (last-position) prompt-position)))
              (edit-sequence defs
                (unless (or (zero? defs-last-position₀)
                            (eqv? (send defs get-character (sub1 defs-last-position₀)) #\newline))
                  (send defs insert "\n" defs-last-position₀ 'same))
                (send this copy-to defs prompt-position (last-position) (send defs last-position))
                (define defs-last-position₁ (send defs last-position))
                (cond [(equal? recording 'copy)
                       (send* defs
                         (tabify-selection defs-last-position₀ defs-last-position₁)
                         (scroll-to-position defs-last-position₁))]
                      [else (send defs set-position (add1 defs-last-position₀) defs-last-position₁
                                  #false #false)
                            (un/comment-selection defs)
                            (send defs set-position defs-start defs-end
                                  #false #false)])))))
         (inner (void) on-submit))

      
       (define prompt-position 0)
       (define/private (prompt-start) (- prompt-position (string-length (get-prompt))))
       (define/private (move-to-prompt) (set-position (get-unread-start-point)))

       ;  ★ Movement before Unread Start Point ★
       (inherit set-position)
       (define/override (insert-prompt) (super insert-prompt)
         (set! prompt-position (get-start-position))
         (style-prompt))
       (define/override (move-position code [extend? #false] [kind 'simple])
         (edit-sequence this
           (super move-position code extend? kind)
           (when (neither extend? ((get-unread-start-point) . < . (get-start-position)))
             (move-to-prompt))))
       (define/augment (after-set-position)
         (edit-sequence this
           (when (and (<= (prompt-start) (get-start-position))
                      (<= (get-end-position) (get-unread-start-point)))
             (move-to-prompt))
           (inner (void) after-set-position)))))))
