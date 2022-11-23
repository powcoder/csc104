#lang snack/pure (provide →ast
                          emfive:markup
                          R:markup
                          python:markup)

(reprovide (only-in "markup.rkt" token/c marked→chunked)
           (only-in "strings-and-images.rkt" space-width))

(require (only-in "layout-dsl.rkt"
                  struct/contract
                  try
                  define-stacker-and-trailer
                  fit-atomic
                  ___
                  ␣ ⦅⦆ 〚〛 ❜
                  =_₎ -≈₎ -≈₎/-_₎)
         
         (multi-in snack {match conditional})
         (submod snack/list core))

(require "../layout-ast.rkt"
         (only-in "../common.rkt" #;new next)) ; only pattern matches.

(struct/contract ^^if-clause ([condition-result (list/c any/c any/c)]))
(struct/contract ^sibling ([term any/c]))
(define/match₁ siblings
  [(list) (list)]
  [(list e) (list e)]
  [(list es ... e-last) (append (map ^sibling es) (list e-last))])

#;(require (only-in snack/port writeln))

#;[atom→token ((not/c list?) . → . (or/c string? image?))]
(require (only-in (combine-in 2htdp/private/image-more mrlib/image-core) image?))
(define (double-quote s) (string-append "\"" s "\""))
(define (emfive:atom→token atom)
  (type-case atom
    [symbol?  (symbol->string atom)]
    [boolean? (if atom "#true" "#false")]
    [string?  (double-quote atom)]
    [number?  (string-append (if (inexact? atom) "#i" "")
                             (number->string atom))]
    #;[Box? (format "~v" atom)]
    [void? "#void"]
    [#:assert image? atom]))
(define (R:atom→token atom)
  (type-case atom
    [symbol?  (symbol->string atom)]
    [boolean? (if atom "TRUE" "FALSE")]
    [string?  (double-quote atom)]
    [number?  (number->string atom)]
    [void? "NULL"]
    [#:assert image? atom]))
(define (python:atom→token atom)
  (type-case atom
    [symbol?  (symbol->string atom)]
    [boolean? (if atom "True" "False")]
    [string?  (double-quote atom)]
    [number?  (number->string atom)]
    [void? "None"]
    [#:assert image? atom]))


(require snack/syntax)

(define (emfive:markup term maximum-width #:compact-fun? compact-fun?)
  
  (define (⟲ term) (emfive:markup term maximum-width #:compact-fun? compact-fun?))
  (define-stacker-and-trailer ≡₎ ---₎/-≈₎/=₎ ⟲ maximum-width) ; ... now only used via ...
  (define-macro (-≡₎:-≈₎/-_₎ head:expr rest:expr) (-≈₎/-_₎ (⟲ head) (≡₎ rest)))
  
  (match term

    [(next term) (___ (⟲ term))]

    [(or (^atom atom) (? symbol? atom)) (fit-atomic maximum-width (emfive:atom→token atom))]
    
    [(^^if-clause condition-result) (---₎/-≈₎/=₎ condition-result)]  
    [(^for-clause name ♯with parts) (〚〛 (if ♯with
                                            (-≡₎:-≈₎/-_₎ name parts)
                                            (---₎/-≈₎/=₎ (list* name parts))))]

    [e (⦅⦆ (match e
             
             [(list f) (⟲ f)] [(list es ...) (---₎/-≈₎/=₎ es)]
             
             [(^definition name ♯variable bind body)
              (try (and ♯variable (-≈₎ (⟲ name) (-≈₎ (⟲ bind) (⟲ body))))
                   (=_₎ (-≈₎/-_₎ (⟲ name) (⟲ bind))
                        (⟲ body)))]
             
             [(^fun header body) (if compact-fun?
                                     (---₎/-≈₎/=₎ (list header body))
                                     (=_₎ (⟲ header)
                                          (␣ (⟲ body))))]
             
             [(^if         name clauses) (-≡₎:-≈₎/-_₎ name (map ^^if-clause clauses))]
             
             [(^with       name definitions body) (=_₎ (-≡₎:-≈₎/-_₎ name definitions)
                                                       (⟲ body))]
             
             [(^for        name clauses) (-≡₎:-≈₎/-_₎ name clauses)]
             [(^block      name (list))  (⟲ name)] ; could be handled by (f) presumably
             [(^block      name es)      (-≡₎:-≈₎/-_₎ name es)]))]))


(define (R:markup term maximum-width #:compact-fun? compact-fun?)
  (define-values (define:R:name fun:R:name) (values '<- 'function))
  (define (⟲ term) (R:markup term maximum-width #:compact-fun? compact-fun?))
  (define-stacker-and-trailer ≡₎ ---₎/-≈₎/=₎ ⟲ maximum-width)  
  (match term
    ; meta
    [(next term) (___ (⟲ term))]
    ; atomic
    [(or (^atom atom) (? symbol? atom)) (fit-atomic maximum-width (R:atom→token atom))]
    
    [(^sibling e) (❜ (⟲ e))]
    [(^definition name #false (list f parameters ...) body)
     (-≈₎ (⟲ f) (-≈₎ (⟲ define:R:name) (⟲ (^fun (list* fun:R:name parameters) body))))]
    [(^definition name ♯variable bind body)
     ; Keep tail  ‘<- expr’ on same line, allow stacking in tail, allow all stackings in tail.
     (-≈₎ (⟲ bind) (---₎/-≈₎/=₎ (list define:R:name body)))]
    [(^if         name (list (list condition consequent)
                             (list else-name alternative)))
     ; if (condition) { consequent }–else {alternative}
     (---₎/-≈₎/=₎ (list name (list '|| condition)
                        '|{| consequent '|} else| '|{| alternative '|}|))]
    [(^fun (list _ parameters ...) body)
     (---₎/-≈₎/=₎ (list (list* fun:R:name parameters) '|{| body '|}|))]
    #;[(list* (^atom 'list) es) (⟲ (list* 'c es))]
    [(list f)                 (-≈₎ (⟲ f) #:tight (⦅⦆ ""))]
    [(list* f es)             (-≈₎ (⟲ f) #:tight (⦅⦆ (---₎/-≈₎/=₎ (siblings es))))]
    
    [(^^if-clause condition-result) (---₎/-≈₎/=₎ condition-result)]    
    [(^for-clause name ♯with parts)
     (〚〛 (if ♯with (-≈₎/-_₎ (⟲ name) (≡₎ parts)) (---₎/-≈₎/=₎ (list* name parts))))]))

(define (python:markup term maximum-width #:compact-fun? compact-fun?)
  (define-values (define:variable:python:name define:function:python:name) (values '= 'def))
  (define (⟲ term) (python:markup term maximum-width #:compact-fun? compact-fun?))
  (define-stacker-and-trailer ≡₎ ---₎/-≈₎/=₎ ⟲ maximum-width)  
  (match term
    ; meta
    [(next term) (___ (⟲ term))]
    ; atomic
    [(or (^atom atom) (? symbol? atom)) (fit-atomic maximum-width (python:atom→token atom))]
    
    [(^sibling e) (❜ (⟲ e))]
    
    [(^definition name #false (list f parameters ...) body)
     (---₎/-≈₎/=₎ (list define:function:python:name (list* f parameters) ': 'return body))]
    [(^definition name ♯variable bind body)
     (-≈₎ (⟲ bind) (---₎/-≈₎/=₎ (list define:variable:python:name body)))]
    
    [(list f)                 (-≈₎ (⟲ f) #:tight (⦅⦆ ""))]
    [(list* f es)             (-≈₎ (⟲ f) #:tight (⦅⦆ (---₎/-≈₎/=₎ (siblings es))))]))
