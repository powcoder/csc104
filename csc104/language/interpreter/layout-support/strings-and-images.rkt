#lang racket/base (require snack/contract
                           (submod snack/list core)
                           (only-in snack/list join partition)
                           racket/math
                           snack/lambda)

(provide (contract-out
          ; step-display communicates to layout.rkt via this, which layout.rkt re-exports
          [space-width (parameter/c positive-natural/c)]
          ; Only layout.rkt for fit-atomic and leading indent parameter, respectively.
          [string-or-image/c predicate/c]
          [strings-and-images/c predicate/c]
          ; Only layout.rkt for measuring atom and leading indent parameter.
          [|| (strings-and-images/c . → . natural/c)]
          ; Only layout.rkt for a chunked column.
          [invisible (strings-and-images/c . → . strings-and-images/c)]))

(require (submod "../common.rkt" strings-and-images))

(define-syntax-rule (define/c . parts) (define . parts))

(define space-width (make-parameter 16))

; Only exported and for contracts.
(define string-or-image/c (or/c string? image?))
(define strings-and-images/c (listof string-or-image/c))

(define (invisible strings-and-images)
  (define-values (strings images) (partitioned strings-and-images))
  (join (if (empty? strings) '()
            (list (make-string (string-length (apply string-append strings)) #\space)))
        (if (empty? images) '()
            (list (rectangle (apply + (map image-width images)) 0 "solid" "transparent")))))

(define (|| strings-and-images)
  (define-values (strings images) (partitioned strings-and-images))
  (exact-ceiling (apply + (join (map string-length strings)
                                (map (λ• (/ (image-width •) (space-width))) images)))))
#;
(contracts [(partitioned strings-and-images/c) → (values (listof string?) (listof image?))])
(define (partitioned strings-and-images) (partition string? strings-and-images))
