#lang snack/pure (require "form-names.rkt")

(provide:#%module-begin)
(provide:#%top-interaction)

(provide:#%datum)
(provide:#%top)

(provide:#%app)
(provide:list)

(as-keywords provide:define
             provide:same! provide:true! provide:false!
             provide:if provide:else provide:and provide:or
             provide:fun provide:anonymous
             provide:for/list provide:each provide:comprehension
             provide:with provide:sequential provide:need! provide:time provide:for
             provide:require provide:only provide:prefix)

(reprovide (except-in "functions/non-image.rkt"
                      list-of)
           "functions/image.rkt"
           (prefix-in  color: "functions/color-constants.rkt")
           (prefix-in colour: "functions/color-constants.rkt"))

(module interpreter racket/base

  (require (only-in "form-names.rkt" as-identifier provide:list))  
  (as-identifier §list-constructor provide:list)
  
  (require reprovide/reprovide)
  (reprovide (only-in "form-names.rkt"
                      [name:ifs      hide:if-introduction]
                      [name:if-steps hide:if-conditions]
                      [name:for      hide:accumulate])))
