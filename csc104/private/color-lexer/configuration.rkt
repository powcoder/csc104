#lang racket/base (provide (all-defined-out))

(require "support.rkt" parser-tools/lex (prefix-in : parser-tools/lex-sre))

; Dispatcher
; ——————————
(define-lexer-pattern (:# pattern ...) (:: "#" pattern ...))

; Forbidden — outside of text and comments
; —————————
(define-lex-abbrevs [forbidden ","])

; Keywords
; ————————
(define-lex-abbrevs [keyword-character ":"])

; White-Space
; ———————————
#;https://en.wikipedia.org/wiki/Whitespace_character#Definition_and_ambiguity
; Copy-paste in DrRacket tends to convert/remove #\r : investigate.
; #\newline ≡ #\linefeed
(define-lex-abbrevs [compthink-whitespace (:or #\space #\newline #\return)])

; Boolean Literals
; ————————————————
(define-lex-abbrevs
  [boolean (:or "F" "T" "false" "true")]
  [boolean-prefix (:or (⫋ "true") (⫋ "false"))])

; Parentheticals
; ——————————————
(define-lex-abbrevs
  [parenthetical-open  (char-set "([{")] 
  [parenthetical-close (char-set ")]}")])
(define (parenthetical? c) (memq c `(#\( #\) #\[ #\] #\{ #\})))

; Double-Quote
; ————————————
(define (double-quote? c) (eq? c #\"))
(define-lex-abbrevs [“ "\""])

; Here-String
; ———————————
(define here-string-open "#\"")
(define here-string-close "\"" #;(apply string (reverse (string->list here-string-open))))
(define-lex-abbrevs [here-string "\""])

; Comments
; ————————
(define (comment-character? c) (eq? c #\;))
(define-lex-abbrevs
  [comment-character ";"]
  [nested-comment-character "|"])
