#lang snack/pure (provide drracket-menu-items
                          drracket-keybindings
                          repl-keybindings)

(require (submod snack/sexp commands)
         (submod snack/text commands)         
         (only-in "drracket.rkt" more-definitions-or-interactions)
         
         (only-in snack/drracket labels:definitions labels:interactions)
         snack/string-constants

         (only-in snack/keybinding
                  add-binding/s augment-binding/s user remove-shortcut
                  alt/option mac-command default-modifier get-default-shortcut-prefix
                  find-keymap-for-function)
         (submod snack/keybinding racket)
         
         (multi-in snack (functional class))
         (submod snack/class implement))

(define add (curry add-binding/s user))

(define (drracket-menu-items frame)
  (define-values (edit-menu view-menu) (values (send frame get-edit-menu)
                                               (send frame get-show-menu )))
  
  ; Remove default shortcuts for Show/Hide of Definitions or Interactions ...
  (remove-shortcut view-menu labels:definitions)
  (remove-shortcut view-menu labels:interactions)
  ;  ... and bind to showing more definitions/interactions:
  (add (default-modifier "d") "show more of definitions"
       (more-definitions-or-interactions #:definitions? #true))
  (add (default-modifier "e") "show more of interactions"
       (more-definitions-or-interactions #:definitions? #false))
  
  ; Remove ‘ctrl-;’ Preferences shortcut from Edit menu (if it exists on the platform) so it can't
  ;  shadow our added “line comment/uncomment around selection” if that chooses ‘ctrl’.
  (remove-shortcut edit-menu (string-constant preferences-menu-item)))

(define (drracket-keybindings)

  ; ★ Un/Comment ★
  (add (default-modifier "semicolon") "line comment/uncomment around selection" un/comment-selection)
  
  ; ★ Sexp ★
  ; Forward / Backward ——— move / select
  (add (list (alt/option "right") "c:]") "go forward by sexp"  forward-sexp)
  (add (list (alt/option "left")  "c:[") "go backward by sexp" backward-sexp)
  (add (list (alt/option "s:right") "s:c:]" "s:c:}" "c:}")
       "select forward by sexp"  select-forward-sexp)
  (add (list (alt/option "s:left")  "s:c:[" "s:c:{" "c:{")
       "select backward by sexp" select-backward-sexp)
  ; Un/Wrap
  (augment-binding/s (racket:get-keymap) "[" "try hug±" try-hug±)
  (augment-binding/s (racket:get-keymap) "]" "try-expand" try-expand)
  ; Explode
  (add '() "explode" explode)
  
  ; ★ Auto-pair ★
  (add "s:|" "maybe insert #||# pair" maybe-insert-#||#-pair)
  (add "‘" "insert ‘’ around selection" (surrounder "‘" "’"))
  (add "“" "insert “” around selection" (surrounder "“" "”")))

(define (repl-return-keybindings chaining-to)
  (define keymap (new keymap:aug-keymap%))
  (add-binding/s keymap "return"   "repl: submit if balanced" repl-maybe-submit)
  (add-binding/s keymap "s:return" "repl: insert newline"     repl-insert-newline)
  #;(add-binding/s keymap "leftbuttondouble" "repl: leftbuttondouble"
                   (λ (interactions event)
                     (println (send keymap get-map-function-table))
                     #;(send interactions copy-prev-previous-expr)
                     #false))
  keymap)

(define (repl-keybindings interactions)
  (let*♯ ([repl-keymap (send interactions get-keymap)])
         (send repl-keymap chain-to-keymap (repl-return-keybindings repl-keymap) #true)
         (let*♯ ([history-keymap (find-keymap-for-function repl-keymap "put-previous-sexp")])
                (send∗ history-keymap map-function
                       [(mac-command "up")   "put-previous-sexp"]
                       [(mac-command "down") "put-next-sexp"]))))
