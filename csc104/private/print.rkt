#lang snack/pure

(#| Display of potential specials, styled. |#
 
 module display racket/base (require reprovide/reprovide)

  (provide (all-defined-out))
  (reprovide (only-in snack/style delta:underline delta:bold delta:underline-bold))
  
  (require "log.rkt" racket/require (multi-in snack (style port symbol)))

  (define (display/default-color s) (display-with-delta s #false))
  
  (define (display-with-delta string delta♯)
    (with-compthink-log-fail (write-special/display (string-snip string #:delta♯ delta♯) string)))

  (define-symbols #:prefix token: comment symbol parenthesis image constant string)

  (define (display/racket-style image/string token-type #:delta♯ [delta♯ #false])
    (with-compthink-log-fail (write-special/display
                              (if (string? image/string)
                                  (string-snip image/string (syntax-style token-type) #:delta♯ delta♯)
                                  image/string)
                              image/string))))

#;{; From distro, ideas for eventually having a click-back on an id to go to the definition.
   (let ([start (send outer-text last-position)])
     (send outer-text insert text)
     (let ([end (send outer-text last-position)])
       (send outer-text change-style style start end #f)
       (when clickback
         (send outer-text set-clickback start end clickback))))
   (send -text change-style clickback-style a b)
   (for ([range (in-list ranges)])
     (define stx (range-stx range))
     (define start (range-start range))
     (define end (range-end range))
     (when (syntax? stx)
       (send output-text set-clickback start end 
             (λ (_1 _2 _3)
               (show-range stx start end)))))
   (λ (str clickback)
     (send e change-style d-http)
     (let* ([before (send e get-start-position)]
            [_ (send e insert str)]
            [after (send e get-start-position)])
       (send e set-clickback before after 
             (λ (a b c) (clickback))
             d-http))
     (send e change-style d-usual))
   drracket/drracket/private/colored-errors.rkt ; deprecated?
   drracket/drracket/private/debug.rkt}

(module custom racket/base (require reprovide/reprovide)
  
  (provide display-styled-chunks
           styled-as-comment)
  (reprovide (only-in (submod ".." display)
                      token:comment
                      token:symbol))
  
  (require (only-in (submod ".." display) display/racket-style)
           snack/string)

  (define (styled-as-comment . ss) (list (apply ~a␣ ss) token:comment))
  
  (define (display-styled-chunks content-style-chunks)
    (for-each (λ (content-style) (apply display/racket-style content-style)) content-style-chunks)))

(module langs racket/base
  (require snack/contract
           (submod "../words/ui.rkt" print)
           (only-in "../shared/code.rkt" printable-value))

  (require (only-in racket/pretty pretty-write pretty-print pretty-print-columns)
           (only-in racket/format ~a))
  
  (provide printable-value truncate 
           display-extra-repl
           (all-from-out racket/pretty)
           (prefix-out compthink:
                       (contract-out [set-printing-parameters ((→ boolean?) (→ any) . → . any)])))
  
  (define (truncate s len) (~a s #:max-width len #:limit-marker message:ellipsis))

  (require (only-in (submod ".." display) display-with-delta display/default-color))

  (require (submod "../configuration.rkt" interactions))
  ;
  (define (display-extra-repl welcome-delta dark-green-delta
                              prefix-types?)
    (define output
      (append
       (list (list configuration:language-version)
             (list message:interactions-area))
       (if prefix-types?
           (list (list message:custom (list message:prefix-types dark-green-delta) message:period))
           (list))))
    (for ([line output])
      (for ([part line]) (apply display-with-delta (if (list? part) part (list part welcome-delta))))
      (newline)))

  (require (submod "../english.rkt" types)
           "is-image.rkt"
           snack/conditional racket/local
           (only-in racket/pretty pretty-print-print-hook pretty-print-size-hook)
           (only-in racket/pretty
                    pretty-print-pre-print-hook pretty-print-post-print-hook
                    pretty-print-print-line
                    #;print-boolean-long-form ; from racket/base
                    pretty-print-show-inexactness pretty-print-exact-as-decimal
                    #;print-as-expression))

  (define (set-printing-parameters print-types? thunk)
    ; from racket/base
    #;(contracts [with-image-and-function-hook ((→ any) . → . any)]
                 #;[prefix-object-type (any/c output-port? . → . void?)]
                 #;[post-print-hook (any/c output-port? . → . void?)]
                 #;[indent-hook ((♯ exact-nonnegative-integer?)
                                 output-port?
                                 exact-nonnegative-integer?
                                 (or/c exact-nonnegative-integer? 'infinity)
                                 ; Hung on this with type prefixing, when hook could produce void.
                                 #;(map list (range 0 10 1) (range 0 10 1))
                                 . → . exact-nonnegative-integer?)])
    (parameterize ([print-boolean-long-form #true]
                   [pretty-print-show-inexactness #true]
                   [pretty-print-exact-as-decimal #true]
                   [print-as-expression #false])
      ; Image as string place-holder if necessary, function as name [color struct as list].
      (define (with-image-and-function-hook thunk)
        (define ((make-hook original do) value display? port)
          (define name (object-name value))
          (cond [(and (not (port-writes-special? port)) (is-image? value)) (do "an image" port)]
                [(and (procedure? value) (symbol? name)) (do (symbol->string name) port)]
                [else (original value display? port)]))
        #;(parameter/c (any/c boolean? output-port? . → . void?))
        #;(parameter/c (any/c boolean? output-port? . → . (♯ exact-nonnegative-integer?)))
        (parameterize ([pretty-print-print-hook (make-hook (pretty-print-print-hook) display)]
                       [pretty-print-size-hook (make-hook (pretty-print-size-hook)
                                                          (λ (s _) (string-length s)))])
          (thunk)))
      ; Optional type prefix.
      (define-values (prefix-object-type post-print-hook indent indent-hook)
        (let ([current-print-object #false])
          (local [(define (value→type v)
                    (type-case v
                      [number? type:number] [boolean? type:boolean] [string? type:text]
                      [list? type:list] [procedure? type:function] [is-image? type:image]
                      [else #false]))]
            (values
             (λ (value port)
               (unless current-print-object
                 (define type-name (value→type value))
                 (when (list? value) (set! current-print-object value))
                 (when type-name (void (parameterize ([current-output-port port])
                                         (display/default-color (format "~a: " type-name)))))))
             (λ (value port) (when (and current-print-object (equal? value current-print-object))
                               (set! current-print-object #false)))
             (make-string (string-length "list:  ") #\space)
             (λ (line port offset width)
               (cond [(and (number? width) (not (equal? 0 line)))
                      (newline port)
                      (cond [current-print-object (display indent port)
                                                  (string-length indent)]
                            [else 0])]
                     [else 0]))))))
      (with-image-and-function-hook
          (λ () #;(contracts [augment-pretty-print-hook
                              ((→ (any/c output-port? . → . void?))
                               ; When this was wrong, checking it hung DrRacket.
                               (any/c output-port? . → . void?)
                               . → . (any/c output-port? . → . void?))])
            (define (augment-pretty-print-hook old-hook new-hook)
              (define old (old-hook))
              (λ (v port) (old v port) (when (print-types?) (new-hook v port))))
            (parameterize
                (; For optionally prefixing printed values with their types.
                 #;(parameter/c (any/c output-port? . → . void?))
                 [pretty-print-pre-print-hook
                  (augment-pretty-print-hook pretty-print-pre-print-hook prefix-object-type)]
                 #;(parameter/c (any/c output-port? . → . void?))
                 [pretty-print-post-print-hook
                  (augment-pretty-print-hook pretty-print-post-print-hook post-print-hook)]
                 #;(parameter/c ((♯ exact-nonnegative-integer?)
                                 output-port?
                                 exact-nonnegative-integer?
                                 (or/c exact-nonnegative-integer? 'infinity)
                                 . → . exact-nonnegative-integer?))
                 [pretty-print-print-line
                  (if (print-types?) indent-hook (pretty-print-print-line))])
              (thunk)))))))


(module step-display racket/base (require snack/contract (submod ".." display))
  (provide (prefix-out print: (combine-out

                               (contract-out [display/default-color (string? . → . any)])
                               display/racket-style
                                                                          
                               delta:underline
                               delta:bold
                               delta:underline-bold
                               
                               token:symbol
                               token:parenthesis
                               token:constant
                               token:string
                               token:image))))
