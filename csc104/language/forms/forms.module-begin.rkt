#lang snack/pure (provide emfive:#%module-begin #;(rename-out [#%module-begin emfive:#%module-begin]))

(require snack/syntax
         (only-in "base.rkt" base:#%app base:#%module-begin)
         (only-in "styled-output.rkt" announce-starting))

(define-syntax-parser emfive:#%module-begin
  [(_) (syntax/loc this-syntax (base:#%module-begin))] ; dummy Interactions, empty Definitions
  [(_ body ...) (syntax/loc this-syntax (base:#%module-begin (base:#%app announce-starting)
                                                             body ...))])
