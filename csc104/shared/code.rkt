#lang snack/pure (provide printable-value
                          (struct-out printable)
                          simply (struct-out simple-code)
                          name-else-value
                          syntax→code
                          syntax→code′)

(require (only-in racket/base [#%app base:#%app]))
#;(require "../private/log.rkt")
(require snack/syntax (for-syntax snack/syntax) syntax/parse)

(define (name-else-value f) (or (object-name f) f))

(define (printable-value v)
  #;(log-compthink-error "~s : ~s : ~s" v (struct->vector v) (printable? v))
  (cond [(compthink:simple-code? v) (object-name v)]
        [(procedure? v) (name-else-value v)]
        [(list? v) `(list . ,(map printable-value v))]
        [else v]))

(struct printable () #:transparent)
(struct simple-code printable (source) #:property prop:object-name 0 #:transparent)
(define-syntax-parser simply
    [(_ expression:expr) (§∘ (base:#%app simple-code '#,(syntax->datum (§ expression))))])
(define (compthink:simple-code? v)
  (and (struct? v) (eq? (vector-ref (struct->vector v) 0) 'struct:simple-code)))

; Convert quoted-closed syntax (see forms.rkt and custom.rkt) to compthink printable value.
(define syntax→code
  (syntax-parser #:literals (quote)
                 [(quote (e:expr ...)) `(list . ,(stx-map syntax→code #'((quote e) ...)))]
                 [(quote e:expr) (handle-datum #'e)]
                 [(_ ...) (stx-map syntax→code this-syntax)]
                 [_ (syntax->datum this-syntax)]))

(define syntax→code′
  (syntax-parser #:literals (quote)
                 [(quote (e:expr ...)) #`(list . #,(stx-map syntax→code′ #'((quote e) ...)))]
                 [(quote e:expr) #`#,(handle-datum #'e)]
                 [(_ ...) #`#,(stx-map syntax→code′ this-syntax)]
                 [_ this-syntax]))

(define (handle-datum e)
  (define datum (syntax->datum e))
  (cond [(procedure? datum) (name-else-value datum)]
        [(list? (object-name datum)) (object-name datum)]
        [else datum]))
