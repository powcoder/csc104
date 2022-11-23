#lang snack/pure (provide (rename-out [-snip% snip%]
                                      [-snip-class snip-class]))

; ★ Configuration : Snip Infrastructure and Look (of look-and-feel).

(define classname-module (format "~s" '(lib "underline-snip.ss"
                                            "csc104")))

(define-values (border-width border-style) (values .75 'hilite))
(define-values (left-right-margin top-margin bottom-margin) (values 0 0 2))

(require (only-in framework racket:text%)
         snack/preferences
         (multi-in snack {gui draw mutation class math})
         (submod snack/class implement)
         racket/gui/base
         racket/class)

(define preference:framework:white-on-black? 'framework:white-on-black?)
(define-values (white black) (values "white" "black"))
(define smoothing:smoothed 'smoothed)

; ★ Snipclass

(define -snip-class% (class snip-class%
                       (define/override (read stream-in)
                         (define snip (make-snip stream-in))
                         (send (send snip get-editor) read-from-file stream-in #false)
                         snip)
                       (define/public (make-snip stream-in) (new -snip%))
                       (super-new)))
  
(define -snip-class (new -snip-class%))
(send∗ -snip-class (set-version 1) (set-classname classname-module))
(send (get-the-snip-class-list) add -snip-class)

; ★ Snip

(define racket:text%-with-copy-self (class racket:text% (inherit copy-self-to)
                                      (define/override (copy-self)
                                        (define ed (new racket:text%-with-copy-self))
                                        (copy-self-to ed)
                                        ed)
                                      (super-new)
                                      (inherited set-max-undo-history 'forever)))

(define -snip%
  (class* editor-snip% (readable-snip<%>)

    ; ★ Editor Snip — mimic parameterization from  editor-snip:decorated%
    
    (inherit get-editor get-style)
    
    (define/override (write stream-out) (send (get-editor) write-to-file stream-out 0 'eof))
    
    (define/override (copy)
      (define snip (make-snip))
      (send snip set-editor (send (get-editor) copy-self))
      (send snip set-style (get-style))
      snip)

    (define/public (make-editor) (new racket:text%-with-copy-self))
    (define/public (make-snip)   (make-object -snip%))
    
    ; ★ Meaning of contents

    #;htdp-lib/stepper/private/xml-snip-helpers.rkt 
    (local-require racket/port syntax/strip-context)
    (define (get-source-name text)
      (if (method-in-interface? 'get-port-name (object-interface text))
          (send text get-port-name)
          (send text get-filename)))    
    (define/public (read-special source line column position)
      #;(make-special-comment "comment")
      (define text (get-editor))
      (define source-name (get-source-name text))
      (define stx (port->list (λ (in) (read-syntax source-name in))
                              (open-input-text-editor text 0 'end values source-name)))
      #;(vector source line column position 1)
      (strip-context
       #`(begin . #,(port->list (λ (in) (read-syntax source-name in))
                                (open-input-text-editor text 0 'end values source-name)))))

    ; ★ Border, margins, alignment.

    (inherit get-inset)

    (define (get-color) (if (preferences:get preference:framework:white-on-black?) white black))

    (define (get-border-pen) (send the-pen-list find-or-create-pen (get-color)
                                   border-width border-style))

    ; Add border
    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      
      (define-values (bl bt br bb bw bh) (values (box 0)
                                                 (box 0)
                                                 (box 0)
                                                 (box 0)
                                                 (box 0)
                                                 (box 0)))      
      (get-extent dc x y bw bh #f #f #f #f)
      (get-inset bl bt br bb)
      (define y′ (+ y (- (unbox bh) (unbox bt) (unbox bb))))

      (super draw dc x y left top right bottom dx dy draw-caret)
      
      (local-context dc ([get-pen       set-pen (get-border-pen)]
                         [get-brush     set-brush transparent-brush]
                         [get-smoothing set-smoothing smoothing:smoothed])
                     (send dc draw-line
                           (+ x (unbox bl)
                              bottom-margin)
                           y′
                           (+ x (- (unbox bw) (unbox bl) (unbox br))
                              (- bottom-margin))
                           y′)))

    ; Adjust for alignment.
    (define/override (get-extent dc x y [w #f] [h #f] [d #f] [s #f] [l #f] [r #f])
      ; To adjust one or both of top space and descent when requested by caller, we need both.
      (define-values (s′ d′) (values (or s (box 0)) 
                                     (or d (box 0))))
      (super get-extent dc x y w h d′ s′ l r)
      ; Move extra space from top to descent, to line up baselines with text outside the snip.
      (define Δ (+ (unbox s′) top-margin))
      (update-box! (+ d′ Δ))
      (update-box! (- s′ Δ))
      ; Contract in 7.4 docs is  ⋯ → void? .
      (void))
    
    (super-new [editor (make-editor)]
               [with-border? #false]
               [left-margin   left-right-margin]
               [right-margin  left-right-margin]
               [top-margin    top-margin]
               [bottom-margin bottom-margin])      
    (inherited use-style-background #true)
    (inherited set-align-top-line #true)

    ; ★ Snipclass
    
    (inherited set-snipclass -snip-class)))
