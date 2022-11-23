#lang racket/base ;  ● A simplified library for defining macros ●

(provide shorthand); — a more suggestive name for non-technical audience
;  Single pattern.
;  Optional clause for literals.
;  One or more templates.
;   • multiple templates are implicitly wrapped with  begin , which is renamed as  multiple
;      to look better when we inspect expansion via  rewrite  ...

(provide rewrite)
; To see one step of expansion for an expression which is a macro use, wrap it with  rewrite 
;  in the Interactions — currently that errors in the Defiitions in the emfive langauge.

(provide (for-syntax (rename-out [... etc.])))
; Pattern and template(s) may use  etc.  for usual ellipsis repetition.
; Since csc104 uses ‘⋯’ a lot for elided code, and ‘...’ to “join” comments and code 
;   into a single sentence/thought, I want something distinct (and “humane”).

(provide (for-syntax variants))
; Patterns may use ...
#;(variants  pattern) ;  ... for an optional pattern, i.e. ...
#;(~optional pattern) ;  ... and ...
#;(variants pattern pattern ...+) ; ... for a choice of patterns, i.e. ...
#;(~or*     pattern pattern ...+)
;  ... where a  variant  pattern may be wrapped in braces ...
#;{sub-pattern ...} ; ... to indicate a splice, i.e. ...
#;(~seq sub-pattern ...)
; Templates may use ...
#;(variants template) ; ... for optional expansion used if its pattern variables were bound, i.e. ...
#;(~?       template) ; ... and ...
#;(variants template₀ template ... default-template)
;  ... for a non-binary choice of templates, skipping non-default with unbound pattern variables,
;  ... where a  variant  template may be wrapped in braces ...
#;{sub-template ...} ; ... to indicate a splice, i.e. ...
#;(~@ sub-template ...)

(require (only-in racket/base [begin multiple])
         syntax/parse/define (for-syntax racket/base syntax/parse syntax/parse/define))

(define-syntax-parser shorthand
  #:datum-literals {literally} ; only using this for one lecture
  [(_ (name:id pattern ...) (~optional (literally literal:id ...))
      (~or* template (~seq templates ...+)))
   #:with the-template (nest #'(~? template (multiple templates ...)))
   #`(define-syntax-parser name
       (~? (~@ #:datum-literals {literal ...}))
       [(_ pattern ...) #'the-template])])

(define-simple-macro (rewrite code:expr) (write (syntax->datum (expand-once #'code))))
;  — a place where choosing the expander's set of core forms for emfive would be of benefit

; Reference: “Syntax Templates in Racket” by Ryan Culpepper.
#;https://thomas.gilray.org/scheme-2019/culpepper.pdf

(module brace-splice racket/base (provide sequable spliceable)
  (require (for-template (only-in syntax/parse ~seq))
           syntax/parse/define syntax/parse)
  (define-syntax-class braced #:attributes {}
    (pattern {pat ...} #:when (eq? #\{ (syntax-property this-syntax 'paren-shape))))
  (define-syntax-class spliceable #:attributes {template}
    (pattern seq:braced #:with -@ (quote-syntax ~@) #:with template #'(-@ . seq))
    (pattern template))
  (define-syntax-class sequable #:attributes {the-pattern}
    (pattern seq:braced #:with the-pattern #'(~seq . seq))
    (pattern the-pattern)))

(begin-for-syntax

  (require 'brace-splice (for-syntax racket/base 'brace-splice))
  
  (define-syntax variants
    (pattern-expander
     (syntax-parser
       [(_ :sequable)      #'(~optional the-pattern)]
       [(_ :sequable ...+) #'(~or*      the-pattern ...)])))

  ; Generalizes  ~?  to more than two templates.
  ;   — not by-value of expanded templates, so can't just be a template meta-function
  ; Uses the same binding for our variants form in templates as in patterns.
  ;   — suggests benefit of having a  prop:meta-function , analogous to  prop:pattern-expander 
  (define (nest stx)
    (define/syntax-parse -? (quote-syntax ~?)) ; or could escape as  (... ~?)  below
    (syntax-parse stx #:literals {(variants variants #:phase 1)
                                  (~? ~?             #:phase 1)}
      [(:variants e:spliceable ...+)
       (nest (syntax/loc this-syntax (-? e.template ...)))]
      [(:~? e1 e2 e3 . es)
       #:with inner (syntax/loc (attribute e2) (-? e2 e3 . es))
       (nest (syntax/loc this-syntax (-? e1 inner)))]
      [(e ...) (datum->syntax stx (map nest (attribute e)) stx stx)]
      [_ stx])))
