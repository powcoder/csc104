#lang racket/base (require snack/contract)

(define/contract-out suppress-type-prefix (parameter/c boolean?) (make-parameter #false))

(module feedback racket/base (require snack/contract
                                      (submod "words/ui.rkt" ui)
                                      snack/preferences
                                      (only-in racket/dict in-dict))

  (provide (prefix-out preference: (contract-out [run:monologue           boolean-preference/c]
                                                 [display:underline:next  boolean-preference/c]
                                                 [display:spacing:compact boolean-preference/c]
                                                 [display:fun:compact     boolean-preference/c]
                                                 [interaction:no-wait     boolean-preference/c]))
           in-dict)

  (define (preference-name category name) (format-symbol "drracket:compthink:~a:~a?" category name))
  (define (step:preference-name name) (preference-name 'step name))
  
  (define-values (run:monologue
                  display:underline:next
                  display:spacing:compact
                  display:fun:compact
                  interaction:no-wait)
    (values (boolean-preference (preference-name 'run 'monologue)              #true)
            (boolean-preference (step:preference-name 'display:underline:next) #true)
            (boolean-preference (step:preference-name 'display:spacing:compact))
            (boolean-preference (step:preference-name 'display:fun:compact)    #false)
            (boolean-preference (step:preference-name 'interaction:no-wait))))

  (define/contract-out run:labels  (listof (cons/c boolean-preference/c string?))
    (list (cons run:monologue           message:run:monologue)))
  (define/contract-out step:labels (listof (cons/c boolean-preference/c string?))
    (list (cons display:underline:next  message:display:underline:next)
          (cons display:spacing:compact message:display:spacing:compact)
          (cons display:fun:compact     message:display:fun:compact)
          (cons interaction:no-wait     message:interaction:no-wait))))
