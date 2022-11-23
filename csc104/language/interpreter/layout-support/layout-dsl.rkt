#lang snack/pure (provide try
                          define-stacker-and-trailer
                          fit-atomic
                          ___
                          ␣ ⦅⦆ 〚〛 ❜
                          =_₎ -≈₎ -≈₎/-_₎)

(reprovide (only-in "markup.rkt" struct/contract)
           (only-in "strings-and-images.rkt" space-width))

(require (only-in "strings-and-images.rkt" ||)
         "markup.rkt")

(require snack/syntax
         (multi-in snack {boolean match definition})
         (except-in snack/false λ-case♯)
         (submod snack/list core))


; ● Related Reasearch ●

;  • “A Pretty But Not Greedy Printer (Functional Pearl)” by Jean-Philippe Bernardy
;  • “Polynomial-Time Optimal Pretty-Printing Combinators with Choice” Anton Podkopaev, Dmitri Boulytchev
;  • Knuth and LaTeX?

;  • “Unifying Parsing and Reflective Printing for Fully Disambiguated Grammars” ?
;  • “Programming Languages : A Domain-Specific Embedded Language for Pretty Printing” ?
;  • “a language-independent code formatting by syntactic matching and templates” ?


; ● Choice ●

; We can afford not to fail, since there's no hard width limit in the Interactions.
; The two core choice operations,  try  and  try∗ , propagate  force?  parameter in their last choice
;  (currently second since binary) for whether it's the last possible branch.
; Choice optimization is in binary  try∗ , by minimum number of lines, prefer second choice if equal.
(define force? (make-parameter #true))
(define-macro (unforced marker:expr) (parameterize ([force? #false]) marker))
(define-macro (try marker:expr marker′:expr) (or (unforced marker) marker′))
(define-macro (try∗ marker:expr marker′:expr) (let* ([m (unforced marker)] [m′ marker′])
                                                (if (m . and . (m′ . ⇒ . (> (number-of-lines m′)
                                                                            (number-of-lines m))))
                                                    m m′)))

; Choice optimization is used only via  -≈₎/=₎ : if stacking here ends up as short it's likely better,
;  with less/uniform width. This has been sufficient for emfive, and fast enough to not bother
;  memoizing nor threading a best-known-length.
; Latter respects stackability's “prefer not”.
(define-macro (-≈₎/=₎ marker:expr {~optional (~and #:indent-under ♯indent-under)} marker′:expr)
  #:with indentation (if (attribute ♯indent-under) #'␣ #'values)
  (try∗ (-≈₎ marker marker′) (=₎ #:by-convention? #false marker (indentation marker′))))
; One of the two public uses, for the common case of the head treated specially with rest indented ...
(define-macro (-≈₎/-_₎ marker:expr marker′:expr) (-≈₎/=₎ marker #:indent-under marker′))

; Failure is decided when fitting an atom against the allowed remaining width.
; Remaining width is based on left-indenters (could include images), and text length of right-indent
;  of “currently” final line (number of closings at final expresson in a sequence).
(define-values (｟s ＃｠) (values (make-parameter (list)) (make-parameter 0)))
(define (fit-atomic maximum-width atom)
  (and (or (force?) ((|| (list atom)) . <= . (maximum-width . - . (|| (｟s)) (＃｠))))
       atom))

; ToDo :
;  • attach closers to last in sequence instead?
;  • first pass to compute non-choice info (to strings, note width, tag “next” boundary and internal)?


; ●  prefix / surround  ●

; ToDo : determine/document significance of  delimit  in the markup language, vs just flow etc.

; Meta (for underlining annotations during stepping), doesn't affect layout.
(define (___ marked) (app♯ delimit 'next-start marked 'next-end))

; For linear  marked , try  marker  with  left-indent adjusted by  marked.
(define-macro (try-prefixed marked:expr marker:expr)
  (parameterize ([｟s (append (｟s) (flat-line marked))]) marker))

; Try an indent by  a linear  marked  followed by  marker, which effectively attaches  marker
;  since it can't get “under” indent, to produce a  flow .
(define-macro (prefix marked:expr marker:expr) (app♯ flow
                                                     marked
                                                     (try-prefixed marked marker)))

; Try an indent by  open  (see note for  prefix  about attachment), with  close   added to current
;  right-indent, to produce a  delimit .
; Use of  surround  is via public specializations below (although some of those are used internally).
(define-macro (surround open:str marker:expr close:str)
  (app♯ delimit
        open
        (try-prefixed open (parameterize ([＃｠ (+ (string-length close) (＃｠))]) marker))
        close))

(define-macro (␣   marker:expr) (surround " " marker ""))
(define-macro (⦅⦆ marker:expr) (surround "(" marker ")"))
(define-macro (〚〛 marker:expr) (surround "[" marker "]"))
(define-macro (❜    marker:expr) (surround "" marker ","))


; ●  –––  ●

; Stacking can be :
;  • forced by width limits
;  • preferred by comparing number of lines used
;  • preferred by convention

; Whether to allow stacking: yes, yes if convention prefers stacking, no : 1 / .5 / 0 .
(define ≡? (make-parameter 1))

; Disallow stacking in the first of a pair, attach the second term on same line, with space or not.
(define-macro (-≈₎ marker:expr {~optional (~and #:tight ♯tight)} marker′:expr)
  #:with separation (if (attribute ♯tight) #'values #'␣)
  (let*♯ ([marked (parameterize ([≡? 0]) marker)]) (prefix marked (separation marker′))))

; If allowed at all, try stacking a pair, with indentation, where the second term ends a sequence.
(define-macro (=_₎ marker:expr marker′:expr) (=₎ #:by-convention? #true marker (␣ marker′)))

; Try stacking a pair, where second term ends a sequence, based on reason and stackability context.
(define-macro (=₎ #:by-convention? by-convention? marker:expr marker′:expr)
  (and ((≡?) . >= . (if by-convention? .5 1))
       (let*♯ ([marked (parameterize ([＃｠ 0]) marker)]
               [marked′ marker′]
               [#:result (stack (list marked marked′))]))))

; Public use of  stacker  is currently to request stacking as a preferred due to convention.

; The trailer, let's call it  ---₎/-≈₎/=₎ , is for a sequence of terms where the head can be
;  considered special but we're also willing to stack vertically.
; It tries and commits to fitting on a line, modulo the last term's preferences if context allows,
;  otherwise optimizes layout of head with stacked rest (without requesting by preference),
;  via  -≈₎/=₎ . For emfive:
;  • function call terms
;  • if's condition-result pair
;  • compact anonymous function
;  • for's clauses

; More precisely, internal  ---₎  takes a list of at least one term and tries all but last while
;  forbiding stacking, with the last term layed out while allowing at most by-convention stacking.

(define-macro (define-stacker-and-trailer stacker:id trailer:id ⟲:id maximum-width)
  (begin (define (stacker #:by-convention? [by-convention? #true] es)
           (match es
             ['() (fit-atomic maximum-width "")]
             [`(,e) (⟲ e)]
             [`(,es (... ...) ,e) (=₎ #:by-convention? by-convention?
                                      (app♯ stack (map♯ ⟲ es))
                                      (⟲ e))]))
         (define-macro (trailer terms:expr)
           (local [(define/match₁ ---₎
                     [`(,t) (parameterize ([≡? (min .5 (≡?))]) (⟲ t))]
                     [`(,t . ,ts) (-≈₎ (⟲ t) (---₎ ts))])]
             (try (---₎ terms)
                  (-≈₎/=₎ (⟲ (first terms)) (stacker #:by-convention? #false (rest terms))))))))
