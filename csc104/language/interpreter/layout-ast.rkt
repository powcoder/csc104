#lang snack/pure (provide →ast ^atom)

(require snack/syntax (submod snack/syntax syntax-classes/core)
         snack/match (submod snack/match data))

(syntax-class two  #:splice #:attributes {} [_ _])
(syntax-class some #:splice #:attributes {} [_ ...+])

(require (only-in "common.rkt" new next)
         (for-meta -1 (only-in "../forms/forms.binding.rkt" fun-shape)))

(define-macro
  (define-non-terminal ast-name:id [attribute-name:id ...+]
    class-name:id {literal:id ...}
    pattern:expr (~optional (~seq #:with with-pattern:expr parse:expr)))
  (begin (provide ast-name)
         (struct ast-name [attribute-name ...] #:transparent)
         (define-syntax-class class-name
           #:attributes {attribute-name ...} #:datum-literals {literal ...}
           (ρ pattern (~? (~@ #:with with-pattern parse))))))

#;(define-non-terminal ^fun [header body] Fun-layout {}
    :fun-shape #:with header #'(name . parameters))

(define-non-terminal ^fun [header body]
  Fun-layout {fun} ((~and (:fun . _) header) body))

(define-non-terminal ^definition [name ♯variable bind body]
  Define-layout {define} (name:define (~and bind (~maybe ♯variable:id)) body))

(define-non-terminal ^if [name clauses] If-layout {if}
  (name:if clause:two ...) #:with clauses (α clause))

(define-non-terminal ^with [name definitions body]
  With-layout {with} (name:with definitions:some body))

(define-non-terminal ^block [name es]
  Block-layout {block} (name:block . es))

(define-non-terminal ^for [name clauses]
  For-layout {accumulate} (name:accumulate . clauses))

(define-non-terminal ^for-clause [name ♯with parts]
  For-Clause-layout {each with}
  (~and ((~or* ♯with:with :each) :id . _)
        (name . parts)))

(struct ^atom (atom) #:transparent)

(define (→ast definition/expression/clause)
  (define-macro (↓   id:id) (    →ast (D id)))
  (define-macro (↓↓↓ id:id) (map →ast (D id)))
  (match definition/expression/clause
    [(new d/e/c) (→ast d/e/c)] [(next d/e/c) (next (→ast d/e/c))]
    [(? list?) (data-parse definition/expression/clause          
                           [:Fun-layout    (^fun            (D header)                      (↓ body))]
                           [:Define-layout (^definition     (D name) (α ♯variable) (D bind) (↓ body))]
                           [:If-layout     (^if             (D name) (↓↓↓ clauses))]
                           [:For-Clause-layout (^for-clause (D name) (α ♯with) (↓↓↓ parts))]
                           [:With-layout   (^with           (D name) (↓↓↓ definitions)      (↓ body))]
                           [:Block-layout  (^block          (D name) (↓↓↓ es))]
                           [:For-layout    (^for            (D name) (↓↓↓ clauses))]
                           [e              (↓↓↓ e)])]
    [atom (define (de-syntax v) (if (syntax? v) (syntax-e v) v))
          (^atom (de-syntax atom))]))
