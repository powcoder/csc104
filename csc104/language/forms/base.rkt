#lang snack/pure

(reprovide (prefix-in base: (only-in racket/base
                                     #%module-begin
                                     [#%plain-app #%app] [#%plain-lambda λ] define
                                     list
                                     and or cond else
                                     #%top-interaction #%top quote
                                     define-values begin0 let-values values
                                     begin let if map
                                     for/list for*/list
                                     print
                                     require prefix-in only-in)))
