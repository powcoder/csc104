#lang snack/pure

(provide dummy-htdp-language-mixin debugger-language-settings-mixin
         #;{htdp-language<%> debugger-language<%>})

#| dummy-htdp-language-mixin : satisfy ‘htdp-language<%>’ interface, whose only use seems to be by
    ‘test-tool.scm’, which creates and enables menu item “Racket / Disable Tests”.
    • we implement six of the eight methods we don't expect to ever be called
    • get-module and get-language-position we override for other clients |#

#| debugger-language-settings-mixin : turn off debugger, which then hides “Debug” toolbar button. |#

(require (multi-in lang {htdp-langs-interface debugger-language-interface})
         snack/syntax
         (submod snack/class implement))

(define dummy-htdp-language-mixin
  (mixin () (htdp-language<%>)
    (define-macro (define-methods {method:id  ...+})
      (begin (define (surprised name)
               (raise (make-exn:fail (format "htdp-language<%> method ~a called unexpectedly" name)
                                     (current-continuation-marks))))
             (define/public (method) (surprised 'method)) ...))
    (define-methods {get-sharing-printing 
                     get-abbreviate-cons-as-list 
                     get-allow-sharing? 
                     get-use-function-output-syntax? 
                     get-accept-quasiquote? 
                     get-read-accept-dot})
    (super-new)))

(define (debugger-language-settings-mixin %)
  (if (implementation? % debugger-language<%>)
      (class* % (debugger-language<%>)
        (init-field [debugger:supported #false])
        (define/override (debugger:supported?) debugger:supported)
        (super-new))
      (class % (init [debugger:supported #false]) (super-new))))
