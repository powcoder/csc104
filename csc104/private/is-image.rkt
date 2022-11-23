#lang racket/base (require (only-in racket/contract contract-out -> any/c))

(provide (contract-out [is-image? (any/c . -> . boolean?)]))

; needs to be unified with similiar contexts

(require (only-in mred image-snip% bitmap%)
         (only-in mrlib/cache-image-snip cache-image-snip%)
         (only-in mrlib/image-core image%)
         (only-in racket/class is-a?))
  
(define (is-image? val) (or (is-a? val image%)            ; 2htdp/image
                            (is-a? val cache-image-snip%) ; htdp/image
                            (is-a? val image-snip%)       ; literal image constant
                            (is-a? val bitmap%)))         ; works in other places, so why not
