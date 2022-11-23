#lang racket/base (provide “ double-quote? text text-bad here-string get-here-string)

(require (only-in "configuration.rkt"
                  “ double-quote?
                  here-string here-string-open here-string-close)
         (only-in "numeric.rkt" digit16)
         (only-in "support.rkt" get-offset type:string type:error)
         parser-tools/lex (prefix-in : parser-tools/lex-sre))

#| The syntax reader, if using the reader extension approach, is in terms of dispatch.
   The color color lexer, ifs using parser-tools/lex, is in terms of longest match.

 If ...
  • other lex patterns don't match more than the dispatch character (or more generally, string), and
  • if they match just the dispatch character/string then they occur after the text pattern(s), and
  • the text patterns cover a prefix of every possible extension,
  ... then the text patterns will act as if dispatching.

 Assuming the colorer reconsiders from the start of the token being edited/extended, onward,
  the lexer must classify any prefix that can be made valid with a change to its next character
  as inside some single token that starts with the prefix. |#

(define-lex-abbrevs
  [” “]
  [⍂ "\\"]
  
  [meta (:or “ ⍂ iso-control)]

  [text (:: “ text-contents ”)]
  [text-contents (:* text-escape (:~ meta))]

  [text-escape (:: ⍂ (:or unicode escape-name))]
  [unicode (:or (:: "U" (:** 1 8 digit16)) (:: "u" (:** 1 4 digit16)))]
  [escape-name (:or ⍂ “ "b" "t" "n" "r")]
  
  [text-bad (:: “ text-contents (:? (:: ⍂ (:? (:: (:or "U" "u") any-char)
                                              (:~ escape-name)))
                                    iso-control))])

(define (get-here-string start input)
  
  (define (special-read-line)
    (define next (peek-char-or-special input))
    (cond [(or (eq? next #\newline) (not (char? next))) null]
          [else (read-char input) (cons next (special-read-line))]))
  
  (define ender here-string-close #;(list->string (special-read-line)))
  (define starter (string-append here-string-open ender))
  (define next (peek-char-or-special input))

  (define (result lexeme type)
    (values lexeme type #false (position-offset start) (get-offset input) #;status:datum))
  
  (cond [(or (equal? ender "") (not (eq? #\newline next))) (result starter type:error)]
        [else (read-char input) 
              (let loop ([acc (list (string-append starter "\n"))])
                (define next-line (list->string (special-read-line)))
                (define next (peek-char-or-special input))
                (cond [(and (char? next) (not (equal? next-line ender)))
                       (read-char input)
                       (loop (cons (string-append next-line "\n") acc))]
                      [else (result (apply string-append (reverse (cons next-line acc)))
                                    (if (char? next) type:string type:error))]))]))

#;(module+ test (require rackunit)
  (define text-lexer (lexer [text lexeme] [any-char #false]))
  (define (T  s) (text-lexer (open-input-string s)))
  (define (T? s) (equal? s (T s)))
  (define (“” s) (string-append "\"" s "\""))
  (check-equal? (T "") 'eof)
  (check-true  (T? (“” "")))
  (check-true  (T? (“” "The quick brown fox, jumped over the lazy dog!")))
  (check-true  (T? (“” "\\n\\r\\t\\b\\\"\\\\")))
  (check-false (T? (“” "The quick brown fox\n jumped over the lazy dog!")))
  (check-false (T? (“” "The quick brown fox\\ jumped over the lazy dog!"))))
