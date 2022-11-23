#lang snack/pure

(module ui racket/base (require "meta.rkt")
  (strings [language-name "CSC 104"]
           [language-settings-label (~a␣ language-name "" "Settings")])
  (strings #:prefixed
           [help "Help"]
           [interactions "Interact"] [history:up "Back"] [history:down "Forth"]
           [definitions "Define"] [layout "Layout"] [record "Record"]
           [insert-image "Image"] [shrink "Shrink"]
           [line-comment "Comment"] [explode "Stack"] [format "Format"] [hug "Hug"])  
  (strings #:prefixed
           [display:underline:next  (step: "underline sub-expressions that are about to change")]
           [display:spacing:compact (step: "display compactly")]
           [display:fun:compact     (step: "display anonymous functions compactly")]
           [interaction:no-wait     (step: "don't wait for response")]
           [run:monologue           (~a␣ “run:”  "show internal monologue")]
           #:with [(step: s ) (~a␣ “step:” s)]
           [“step:” "step:"] [“run:”  "Run:"]))

(module print racket/base (require "meta.rkt")
  (reprovide (only-in "general.rkt" message:ellipsis message:period))
  (strings #:prefixed
           [interactions-area "Interactions area."]
           [custom "Custom: "] [prefix-types "Prefix result values with their types"]))

(module step-display racket/base (require "meta.rkt" "general.rkt")
  (provide (contract-out [elided ([] [(♯ string?)] . →∗ . string?)]))
  (strings [header (~a␣ "●" "Steps" "●")] [to-step-lead "○"] [step-lead "•"]
           [too-many-steps "too many steps to show"])
  (define (elided [reason #false]) (~a␣ message:ellipsis (or reason ""))))

(module settings-panel racket/base (require "meta.rkt")
  (strings [output-style "Output Style"] [prefix-type "Prefix result values with their types"]))
