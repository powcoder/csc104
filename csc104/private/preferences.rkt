#lang racket/base #| General DrRacket Preferences Set by Us on First-Ever Loading |#

(provide set-our-preferences! preferences:get)

(require snack/preferences)

(define first?-preference 'compthink:first?)

(require (only-in racket/draw get-face-list))

(define (set-our-preferences!)

  (unless (preferences:default-set? first?-preference)
    (preferences:set-default first?-preference #true boolean?))
  
  (when (preferences:get first?-preference)
    (preferences:set first?-preference #false)

    (preferences:set 'drracket:tools-configuration
                     (list* '(((lib algol60) (tool.rkt)) skip)
                            '(((lib swindle) (tool.rkt)) skip)
                            (preferences:get 'drracket:tools-configuration)))

    (preferences:set 'drracket:child-only-memory-limit
                     (max (preferences:get 'drracket:child-only-memory-limit)
                          (* 1024 1024 500)))

    (preferences:set 'drracket:defs/ints-horizontal #true)

    (preferences:set 'drracket:coverage-show-overview-bar #f)

    (preferences:set 'framework:paren-color-scheme 'spring)
    (preferences:set 'framework:fixup-open-parens #true)
    (preferences:set 'framework:automatic-parens #true)
    (preferences:set 'framework:highlight-parens #true)
    (preferences:set 'framework:fixup-parens #true)
    (preferences:set 'framework:paren-match #true)
    #;(preferences:set-default 
       'framework:tabify
       (list defaults-ht #rx"^begin" #rx"^def" #rx"^(for\\*?(/|$)|with-)" #f)
       (list/c (hash/c symbol? (or/c 'for/fold 'define 'begin 'lambda) #:flat? #t)
               (or/c #f regexp?) (or/c #f regexp?) (or/c #f regexp?) (or/c #f regexp?)))
    ;
    (preferences:set 'framework:square-bracket:cond/offset
                     (append '(("if" 0) ("big-bang" 1))
                             (preferences:get 'framework:square-bracket:cond/offset)))

    #;(preferences:set 'framework:ask-about-paste-normalization #false)
    #;(preferences:set 'framework:do-paste-normalization #false)

    ; get-face-list: [(or/c 'mono 'all)] [#:all-variants? any/c] -> (listof string?)
    (let ([installed-fonts (get-face-list 'mono)] [our-font "Source Code Pro"])
      (when (member our-font installed-fonts)
        (preferences:set 'framework:standard-style-list:font-name our-font)))))
