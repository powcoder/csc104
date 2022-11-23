#lang snack/pure (provide (all-defined-out) token/c)
  
(require (only-in (submod "../token.rkt" layout) token/c)
         "strings-and-images.rkt"
         (multi-in snack {contract  
                          syntax functional match})
         (submod snack/list core)
         (only-in snack/list filter-not)
         (submod snack/match data))
  
(define-syntax-parser struct/contract
  [(_ (name super) ([id contract] ...)) #'(struct name super [id ...])]
  [(_ name ([id contract] ...)) #'(struct name [id ...])])

(struct marked () #:transparent)
(define layout/c (or/c token/c marked?))
(struct/contract (stack   marked) ([markeds (listof layout/c)]))
(struct/contract (flow    marked) ([marked₀ layout/c #;unstacked] [marked₁ layout/c]))
(struct/contract (delimit marked) ([open  (or/c 'next-start string?)]
                                   [marked layout/c]
                                   [close (or/c 'next-end   string?)]))

; For linear content, remove syntactic and semantic markup.
(define/match₁ flat-line
  [(flow marked marked′) (append (flat-line marked) (flat-line marked′))]
  [(delimit 'next-start marked 'next-end) (flat-line marked)]
  [(delimit open marked close) (*list* open (flat-line marked) close)]
  [string/image (list string/image)])

(define (marked→chunked marked [visible-lead (list)])
  (define meta? symbol?)
  (define ⟲ (λ• (marked→chunked • visible-lead)))
  (match marked
    [(flow linear hanging)
     (with-data*
         ([(visible-lead ...) visible-lead]
          [{(line₀ ...)} (⟲ linear)]
          [{(line₁ ...)
            lines ...} (marked→chunked hanging (data (visible-lead ... line₀ ...)))])
       (data {(line₀ ... line₁ ...)
              lines ...}))]
    [(delimit open marked close)
     (with-data ([{lines ...
                   (line ...)} (⟲ (flow open marked))] [close close])
       (data {lines ...
              (line ... close)}))]
    [(stack (list marked₀
                  markeds
                  ...))
     (with-data* ([{line
                    ...} (⟲ marked₀)]
                  [({(line′ ...)
                     ...}
                    ...) (map marked→chunked markeds)]
                  [(indentation ...) (invisible (filter-not meta? visible-lead))])
       (data {line
              ...
              (indentation ... line′ ...)
              ...
              ...}))]
    [string/image (with-data ([string/image string/image]) (data {(string/image)}))]))

(define number-of-lines (∘ length marked→chunked))
