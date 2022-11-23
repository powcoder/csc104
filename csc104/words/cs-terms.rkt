#lang snack/pure (require "meta.rkt")

; • Parenthesized Structure
(strings
 [“parentheses” "parentheses"]
 [“open-parenthesis” (~a␣ "open" “parenthesis”)]
 [“parenthesized” "parenthesized"]
 #:with [“parenthesis” "parenthesis"])

; • Program Grammar
(strings
 [term:expression "expression"]
 [term:literal "literal"] [term:value "value"]
 [term:call "call"] [term:argument "argument"]

 [definition "definition"] [term:defined "defined"] [undefined "undefined"]

 [keyword "keyword"]
   
 [term:name "name"]
 [term:variable "variable"] [term:parameter "parameter"] [body "body"]
   
 [term:condition "condition"] [term:consequent "consequent"] [term:alternative "alternative"])

; • Types
(strings [function "function"]
         [unary-function  (~a␣ “unary” function)]
         [binary-function (~a␣ “binary” function)]           

         [boolean "boolean"]

         [number "number"]
         [integer "integer"]
         [natural-number (~a␣ “natural” number)]

         #:with
         [“unary” "unary"]
         [“binary” "binary"]
         [“natural” "natural"])

(strings [text "text"] [character "character"])
