#lang snack/pure (provide exn→rewritten-message) ; 100ms

#;(require (compthink:log))

(require (only-in (submod "english.rkt" rewrite) λ₁
                  for-the-nth-argument
                  read-syntax:text:image
                  read-syntax:illegal-character)
         (submod "language/functions/error.rkt" rewrite))

(define (exn→rewritten-message exn #:rich? [rich? #false])
  #;(log-compthink-debug "exn→rewritten-message")
  (define original (exn-message exn))
  (define original′ (if rich? original (regexp-replace* "‸" #;"«|»" original " ")))
  (cond [(exn:fail? exn) #;(log-compthink-debug "message: ~a" original)
                         (rewrite original′)] 
        [else original′]))

(define (rewrite msg)

  ; unbound
  #;(define undefined " ~a : is undefined")
  (define define:not-yet " ~a : needs to be defined before being used")
  (define define:none    " ~a : needs to be defined" )
  
  (define replacements
    (list

     ; ● unbound
     (list #rx"^(.*): unbound identifier"
           (λ (_ one) (format define:none one)))
     (list #rx"([^\n]*): undefined;\n cannot reference an identifier before its definition"
           (λ (_ one) (format define:not-yet one)))
     (list #rx"^(.*): undefined;\n cannot use before initialization"
           (λ (_ one) (format "local variable used before its definition: ~a" one)))
          
     ; ● function call : dynamic
     (list #rx"application: not a procedure;\n [^\n]*?\n  given: ([^\n]*)(?:\n  arguments[.][.][.]:(?: [[]none[]]|(?:\n   [^\n]*)*))?"
           (λ (_ one) (format "function call: needed a function to call, but received ~a" one)))
     
     ; ★ read-syntax : static
     (list (regexp-quote "read-syntax: found non-character while reading a string")
           read-syntax:text:image)
     (list #rx"read-syntax: illegal use of `(.)`"
           read-syntax:illegal-character)
     ; ★ "n extra part[s]" : static
     (list #px"(\\d+) extra part" (λ (_ n) (format "~a extra part" (number n))))
     ; ★ suppress "procedure " in front of erroring function's name
     (list #rx"^procedure " suppress)
     ; ★ arity
     (list #px"([^\n]*): arity mismatch;\n[^\n]*\n  expected[^:]*: at least (\\d+)\n  given[^:]*: (\\d+)(?:\n  arguments[.][.][.]:(?:\n   [^\n]*)*)?"
           (λ (all name m actual) (arity-message name m "+inf.0" actual)))
     (list #px"([^\n]*): arity mismatch;\n[^\n]*\n  expected[^:]*: (\\d+)\n  given[^:]*: (\\d+)(?:\n  arguments[.][.][.]:(?:\n   [^\n]*)*)?"
           (λ (all name m actual) (arity-message name m m actual)))
     (list #px"([^\n]*): arity mismatch;\n[^\n]*\n  expected[^:]*: (\\d+) to (\\d+)\n  given[^:]*: (\\d+)(?:\n  arguments[.][.][.]:(?:\n   [^\n]*)*)?"
           (λ (all name m M actual) (arity-message name m M actual)))
     
     ; ★ ‘require’
     (list #rx"standard-module-name-resolver: collection not found\n  for module path: ([^\n]*)(?>.|\n)*"
           (λ (_ one) (format "require: couldn't find anything called ~a" one)))
     (list #rx"(?>[^\n:]*):(?>[^\n:]*):(?>[^\n:]*): cannot open module file\n  module path: (?>[^\n]*)\n  path: ([^\n]*)\n  system error: No such file or directory; errno=2"
           (λ (_ one) (format "require: couldn't find a file called ~a"   one)))
     ; ★ suppress info about other arguments
     (list      #rx"; other arguments were:.*" suppress)
     (list #px"(?:\n  other arguments[.][.][.]:(?:\n   [^\n]*)*)" suppress)
     ; ★ from racket contract
     (list #px"contract violation\n  expected: (.*?)\n  given: ([^\n]*)(?:\n  argument position: ([^\n]*))?"
           contract-error-message)
     ; ★ expecting [argument] of type
     (list  #rx"expects argument of type (<([^>]+)>)" expects₂)
     (list #rx"expected argument of type (<([^>]+)>)" expects₂)
     (list              #rx"expects type (<([^>]+)>)" expects₂)
     (list             #rx"expected type (<([^>]+)>)" expects₂)
     ;
     (list #px"as (\\d+)(?>st|nd|rd|th) argument"
           (λ (_ one) (for-the-nth-argument (string->number one))))
     ; ★ given → received
     ; "above: expects an image as first argument, given 104"
     (list "[,|;] given:? " (λ₁ ", but received "))
     ; ★ simplify ways an image might be represented
     (list (regexp-quote "#<image>") →image)
     (list (regexp-quote  "<image>") →image)
     (list (regexp-quote "#(struct:object:image% ...)") →image)
     (list (regexp-quote "#(struct:object:image-snip% ...)") →image)
     (list (regexp-quote "#(struct:object:cache-image-snip% ...)") →image)
     ; image too big to draw
     (list #rx"[^\n]*drawing context is not ok[^\n]*" "display error: image too big to draw")
     ; ★ types of number
     (list (regexp-quote  "expects a real") →number)
     (list (regexp-quote "expected a real") →number)
     ; Sometimes error messages say "expected: integer" instead of "expected: integer?",
     ;  so it isn't rewritten properly. (Why?)
     (list  (regexp-quote "expects integer") →integer)
     (list (regexp-quote "expected integer") →integer)
     ; ★ remove mention of allowing cylic lists, add english for "n or more items"
     (list (regexp-quote "list or cyclic list") (λ₁ "list"))
     (list #px"a list with (\\d+) or more items"
           (λ (_ n) (format "a list with ~a or more items" (number n))))))
  
  (regexp-replaces msg replacements))
