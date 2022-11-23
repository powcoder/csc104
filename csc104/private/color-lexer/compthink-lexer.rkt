#lang snack/pure (require racket/contract syntax-color/lexer-contract)

(provide (contract-out [compthink-lexer lexer/c]))

(require (only-in "configuration.rkt"
                  :#
                  forbidden
                  compthink-whitespace
                  keyword-character
                  parenthetical-open parenthetical-close parenthetical?
                  boolean boolean-prefix)
         "numeric.rkt" "text.rkt" "comment.rkt"
         "support.rkt"
         parser-tools/lex (prefix-in : parser-tools/lex-sre)
         (multi-in snack (syntax symbol definition conditional function)))

(define-macro (R type:expr {~optional {~seq #:lexeme lex}})
  (ret {~? lex lexeme} type #false start-pos end-pos))
(define-macro (lexeme-error) (R type:error))
(define-macro (parenthetical)
  (ret lexeme type:parenthesis (string->symbol lexeme) start-pos end-pos))

(define-macro (not-expression-text type:expr) (R type #:lexeme ""))

(define-symbols #:prefix peek: 'special)
(define (peek input) (peek-char-or-special input 0 peek:special))

(define (special-follows? input) (eq? peek:special (peek input)))

(define-macro (type-unless-before-special type:expr) (unless-before-special (R type)))
(define-macro (unless-before-special result:expr) (if (special-follows? input-port)
                                                      (lexeme-error)
                                                      result))

(define-lex-abbrevs

  [immediate-terminator (:or compthink-whitespace
                             parenthetical-close)]
  [gapped-terminator    (:or forbidden (:#) parenthetical-open “ comment-character)]

  [terminated (:or parenthetical-close text (:# boolean))] ; and special
  
  [id-delims (:or gapped-terminator immediate-terminator)]
  
  [id-char (:~ id-delims)]
  
  [identifier (:+ id-char)]
 
  [keyword (:: keyword-character identifier)]

  [boolean-bad boolean-prefix])

(define compthink-lexer
  (lexer

   [(:+ compthink-whitespace) (R type:white-space)]
   [forbidden (lexeme-error)]
   [(special) (local [(define next (peek input-port))]
                (cond [(or (eq? next peek:special)
                           (and (char? next) ((lexer [(:or gapped-terminator id-char) #true]
                                                     [any-char #false])
                                              (open-input-string (string next))))) 
                       (read-char-or-special input-port)
                       (values "" type:error #false
                               (position-offset start-pos) (get-offset input-port))]
                      [else (type-case lexeme
                              [number? (R type:constant)]
                              [string? (R type:string)]
                              [else (not-expression-text type:no-color)])]))]
   #;[(special-comment) (not-expression-text type:comment)]
   [(eof) (values lexeme type:eof #false #false #false)]
   
   [parenthetical-open (parenthetical)]

   [(:: terminated (:or gapped-terminator id-char)) (lexeme-error)]
   
   [(:: (:or identifier (:# keyword) ; these grab id-char, so allow or move to break ties with valid
             decimal 
             binary hexadecimal (:# special-number))
        gapped-terminator)
    (lexeme-error)]
   
   [parenthetical-close (unless-before-special (parenthetical))]
   [text                     (type-unless-before-special type:string)]
   [(:# boolean)             (type-unless-before-special type:constant)]
   ;
   [identifier               (type-unless-before-special type:symbol)]
   [(:# keyword)             (type-unless-before-special type:hash-colon-keyword)]
   ;
   [decimal                  (type-unless-before-special type:constant)]
   ;
   [(:or binary hexadecimal) (type-unless-before-special type:constant)]
   [(:# special-number)      (type-unless-before-special type:constant)]

   [text-bad (lexeme-error)]
   [(:# boolean-bad) (lexeme-error)]
   [(:# keyword-character) (lexeme-error)]

   [(:or special-number-bad inexact-bad decimal-bad binary-bad hexadecimal-bad) (lexeme-error)]

   [(:# nested-comment-character) (read-nested-comment input-port start-pos)]
   [(:# here-string)              (get-here-string start-pos input-port)]

   [comment-character      (read-line/skip-over-specials input-port start-pos)]
   [(:# comment-character) (R type:sexp-comment)]

   [(:: any-char) (lexeme-error) #;(extend-error)]))

#;(define-macro (extend-error)
    (local [(define next (peek-char-or-special input-port 0 peek:special))
            (define get-chunk (lexer [(:+ (:~ id-delims)) (values lexeme end-pos)]))]
      (cond [(or (char-whitespace?   next)
                 (parenthetical?     next)
                 (double-quote?      next)
                 (comment-character? next)
                 (memq next `(special ,eof)))
             (ret lexeme type:error #false start-pos end-pos)]
            [else (define-values (rest end-pos) (get-chunk input-port))
                  (R type:error #:lexeme (string-append lexeme rest))])))
