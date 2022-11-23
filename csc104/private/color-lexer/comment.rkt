#lang racket/base (provide comment-character comment-character?
                           nested-comment-character
                           read-line/skip-over-specials
                           read-nested-comment)

(require (only-in "configuration.rkt"
                  :#
                  comment-character comment-character?
                  nested-comment-character)
         (only-in "support.rkt" ret type:comment type:error get-offset)
         parser-tools/lex (prefix-in : parser-tools/lex-sre))

(define (read-line/skip-over-specials input start-pos)
  (values (apply string (let loop ()
                          (define next (peek-char-or-special input))
                          (cond [(or (eq? next #\newline) (eof-object? next)) null]
                                [else (read-char-or-special input) 
                                      (if (char? next) (cons next (loop)) (loop))])))
          type:comment
          #false
          (position-offset start-pos) (get-offset input)
          #;status:continue))

(define-lex-abbrevs [meta (:or (:#) nested-comment-character)])
  
(define get-next-comment
  (lexer [(:: (:#) nested-comment-character) (values  1 end-pos)]
         [(:: nested-comment-character (:#)) (values -1 end-pos)]
         [(:or meta (:* (:~ meta))) (get-next-comment input-port)]
         [(special)                 (get-next-comment input-port)]
         [(special-comment)         (get-next-comment input-port)]
         [(eof) (values 'eof end-pos)]))

(define (read-nested-comment input start [num-opens 1])
  (define-values (diff end) (get-next-comment input))
  (define (R type) (ret "" type #false start end #;status:continue))
  (case diff
    [(eof) (R type:error)]  
    [else (define next-num-opens (+ diff num-opens)) 
          (if (zero? next-num-opens)
              (R type:comment) 
              (read-nested-comment input start next-num-opens))]))
