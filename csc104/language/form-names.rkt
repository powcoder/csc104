#lang snack/pure ; Associate an Emfive name, ~fixed internal name, module plus implementation binding.

; An individual specification below is ...
#;[as:id reference-as:id module implementation-name:id]

; Emfive bindings.
; Clients without-gui.rkt steps.rkt universe.rkt provide them via ...
#;(provide:reference-as) ; ... or, to automatically note in a  keywords  sub-module, ...
#;(as-keywords provide:reference-as ...)

; Keywords for color lexer.
; Gathered  by  keywords.rkt from  keywords  sub-modules of without-gui.rkt steps.rkt universe.rkt
;  that export a list of symbols  emfive-keywords , produced by the use of  as-keywords  or manually.
; Symbolic names are available manually as ...
#;name:reference-as

; Stepper.
; ToDo : document usage — see steps.rkt .

; Breakages of this abstraction layer.
; ToDo : review and reconsider.
;  • display-code.rkt
;  • interpreter/

(reprovide (only-in "forms/form-naming.rkt" as-keywords as-identifier))

(require (only-in "forms/form-naming.rkt" define-provider map-macro))

(map-macro define-provider

           [#%module-begin #%module-begin "forms.module-begin.rkt" emfive:#%module-begin]
           [#%top-interaction #%top-interaction "forms.implicit.rkt" compthink:#%top-interaction]

           [#%datum #%datum racket/base #%datum]
           #;[#%datum #%datum "forms.implicit.rkt" compthink:#%datum]
           #;[quote   quote   "forms.implicit.rkt" compthink:quote]
           
           [#%top #%top racket/base #%top]
           
           [#%app #%app "forms.app.rkt" emfive:#%app]
           
           [step  step  "step.rkt" step]
           [steps steps "step.rkt" steps]
           [R:step  R:step "step.rkt" R:step]
           [python:step  python:step "step.rkt" python:step]
           [hide  hide  "step.rkt" hide]
           [show  show  "step.rkt" show]
           [if-introduction  ifs      "step.rkt" emfive:if-introduction]
           [if-conditions    if-steps "step.rkt" emfive:if-conditions]

           [define define "forms.binding.rkt" compthink:define]

           [same!  same!  "forms.conditional.rkt" compthink:same!]
           [true!  true!  "forms.conditional.rkt" compthink:true!]
           [false! false! "forms.conditional.rkt" compthink:false!]

           [list list "forms.list.rkt" emfive:list]

           [if   if   "forms.conditional.rkt" emfive:if]
           [else else "forms.conditional.rkt" emfive:else]

           [and and "forms.conditional.rkt" emfive:and]
           [or  or  "forms.conditional.rkt" emfive:or]

           [fun fun "forms.binding.rkt" emfive:fun]

           [anonymous anonymous "forms.reflection.rkt" emfive:anonymous]

           [list-of for/list "forms.binding.rkt" compthink:for/list]
           [each    each     "forms.binding.rkt" compthink:each]
           [comprehension comprehension "forms.reflection.rkt" emfive:comprehension]

           [with       with       "forms.binding.rkt"    compthink:with]
           [sequential sequential "forms.reflection.rkt" compthink:sequential]
           [need!      need!      "forms.assume.rkt"     compthink:need!]
           [time       time       "forms.misc.rkt"       compthink:time]
           [accumulate for        "forms.binding.rkt"    compthink:for]

           [require require "forms.require.rkt" compthink:require]
           [only-in    only    "forms.require.rkt" compthink:only-in]
           [prefix  prefix  "forms.require.rkt" compthink:prefix-in]

           [big-bang big-bang "universe.rkt" compthink:big-bang])
