#lang racket/base (provide term⊙/c ⊙ plug plugger)

(define term⊙/c syntax?)

(require racket/contract
         (only-in (submod snack/list core) append-map first rest)
         syntax/parse)

(struct hole () #:transparent)

(define/contract ⊙ term⊙/c (datum->syntax #false (hole)))

(define/contract (plug part inserts) (term⊙/c (listof any/c) . -> . any/c)
  (define-values (plugged _) (plug′ part inserts)) plugged)

(define ((plugger context) insert) (plug context insert))

(define/contract (plug′ part inserts) (term⊙/c (listof any/c) . -> . (values any/c (list/c)))
  (syntax-parse part
    [_ #:when (hole? (syntax-e this-syntax)) (values (first inserts) (rest inserts))]
    [(part . parts) (define-values (plugged  leftovers)  (plug′ #'part  inserts))
                    (define-values (plugged+ leftovers+) (plug′ #'parts leftovers))
                    (values #`(#,plugged . #,plugged+) leftovers+)]
    [_ (values part inserts)]))

  
