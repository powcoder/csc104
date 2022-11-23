#lang racket/base (provide (all-defined-out))

(define (expand-for-structure stx) (local-expand stx 'expression '()))
(define (expand-for-structure∗ stx/list)
  (if (list? stx/list) (map expand-for-structure stx/list) (expand-for-structure stx/list)))

(define (abstractly stx a) (syntax-property stx 'abstractly a))

(define (abstract stx)
  (if (list? stx)
      (map abstract stx)
      (or (and (syntax? stx) (syntax-property stx 'abstractly))
          stx)))

(require (for-template snack/syntax)
         snack/syntax racket/struct)

(define-macro (^ a:id) (abstract (attribute a)))
(define-macro (ξ a:id) (expand-for-structure∗ (attribute a)))

; Prefabs are cross-phase persistent, but keep in mind that embedding as quoted syntax
;  to preserve the actual syntax in the fields recursively turns all parts into syntax.
; Could quote syntactic parts and drop in a literal that creates the structure (is that our approach
;  for  close  in forms.binding.rkt?). Although how much do we care about syntactic parts?

(struct ^literal (datum) #:prefab)
(struct ^list (elements) #:prefab)

(struct ^call (f arguments) #:prefab)

(struct ^definition (name ♯parameters body) #:prefab)
(struct ^fun (parameters body) #:prefab)
(struct ^with (names bodies body) #:prefab)

(struct ^or  (conditions) #:prefab)
(struct ^and (conditions) #:prefab)
(struct ^if  (conditions results else-result) #:prefab)

(struct ^for      (e-id s a-id⒮ a⒮ body unary?) #:prefab)
(struct ^for/list (e-id s           body condition) #:prefab)

(require (for-template racket/base))
(define (serialize a)
  (define ♯prefab-key (prefab-struct-key a))
  (cond [(symbol? ♯prefab-key) #`(make-prefab-struct '#,♯prefab-key
                                                     . #,(map serialize (struct->list a)))]
        [(list? a) #`(list . #,(map serialize a))]
        [else #`#'#,a]))
