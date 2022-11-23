#lang racket/base (provide tool@)

(require (submod "configuration.rkt" interactions))

(require (only-in (combine-in "private/language-ui.rkt") compthink-ui@))

#;(require (lib "2htdp/universe.rkt")
           (prefix-in #%%: (lib "csc104/language/without-gui.rkt")))

(require (only-in "shared-settings.rkt" suppress-type-prefix))

(require "continuation-key.rkt"
         (only-in lang/htdp-langs-save-file-prefix htdp-save-file-prefix)
         (only-in "run-teaching-program.rkt" create-empty-module expand-teaching-program)
         (only-in "private/htdp-stepper-debugger.rkt"
                  dummy-htdp-language-mixin debugger-language-settings-mixin)
         (prefix-in et: errortrace/stacktrace))

(require (submod "test-display.rkt" langs))

(require (only-in "private/settings-panel.rkt" setup-config-panel)
         (only-in snack/structs define-settings-struct))

(require "private/preferences.rkt")

(require (only-in "rewrite-error.rkt" exn→rewritten-message)
         (submod "private/print.rkt" langs)
         (submod "private/print.rkt" custom))

(require (only-in racket/gui/base current-eventspace queue-callback style-delta%)
         (for-syntax racket/base))

(require racket/class
         (only-in racket/list second split-at empty? first rest)
         (only-in racket/string string-join) racket/splicing
         snack/functional snack/conditional snack/definition
         syntax/srcloc
         racket/string)

(define test:render-value/format:width 40)

(require racket/unit drracket/tool)
(define-unit tool@ (import drracket:tool^) (export drracket:tool-exports^)
  (invoke-unit compthink-ui@ (import drracket:tool^))

  (define drs-eventspace (current-eventspace))

  (define (phase1) (void))
  
  (define (phase2) (set-our-preferences!) ; Set some preferences if first time ever loading the tool.
    
    ; Compthink language DrRacket registration.
    (define compthink-language%
      (debugger-language-settings-mixin
       ((drracket:language:get-default-mixin)
        (language-extension
         (drracket:language:module-based-language->language-mixin
          (module-based-language-extension
           (drracket:language:simple-module-based-language->module-based-language-mixin
            (dummy-htdp-language-mixin
             (class drracket:language:simple-module-based-language% (init-field reader-module)
               (super-new))))))))))
    (define compthink-language (instantiate compthink-language% ()
                                 ; Leaving in for some flexibility:
                                 [reader-module '(lib "2019-fall-reader.rkt" "csc104")] ; save/load
                                 ; All the drracket:language:simple-module-based-language% inits.
                                 [language-position configuration:language-position]
                                 [language-numbers  configuration:language-numbers]
                                 [one-line-summary  configuration:one-line-summary]
                                 [language-id "csc104:2019-fall"]
                                 [language-url configuration:course-url]
                                 [reader read-syntax]
                                 [module '(lib "csc104/2019-fall.rkt")]))
    (drracket:language-configuration:add-language ; Register for DrRacket's “Choose Language”.
     #:allow-executable-creation? #false compthink-language))
  (splicing-local [; Default settings
                   (define case-sensitive   #true)
                   (define printing-style   'print #;'constructor)
                   (define fraction-style   'repeating-decimal)
                   (define show-sharing     #false)
                   (define insert-newlines  #true)
                   (define annotations      'none)]
    (define-settings-struct compthink-lang-settings drracket:language:simple-settings
      ([prefix-types? #false])
      case-sensitive printing-style fraction-style show-sharing insert-newlines annotations))

  ; Change default settings, config panel, set more parameters during `on-execute', override printing.
  (define (module-based-language-extension super%)
    (class* super% ()
      
      (define/override (default-settings) (make-compthink-lang-settings #false))
      (define/override (default-settings?   s) (not (compthink-lang-settings-prefix-types? s)))
      (define/override (marshall-settings   s) (compthink-lang-settings->hash s))
      (define/override (unmarshall-settings m) (cond [(hash? m) (hash->compthink-lang-settings m)]
                                                     [else (default-settings)]))
      
      ; Set up “Show Details” pane in “Language > Choose Language...” for “CSC 104”.
      (define/override (config-panel parent)
        (define get/set (setup-config-panel parent))
        (case-lambda [() (apply make-compthink-lang-settings (get/set))]
                     [(settings) (get/set (compthink-lang-settings-prefix-types? settings))]))

      (define/override (on-execute settings run-in-user-thread)
        (define drs-namespace (current-namespace))
        (define scheme-test-module-name ((current-module-name-resolver)
                                         '(lib "test-engine/racket-tests.ss") #false #false #true))
        (define tests-on? (preferences:get 'test-engine:enable?))
        (run-in-user-thread
         (λ ()
           
           (when (getenv "PLTDRHTDPNOCOMPILED") (use-compiled-file-paths '()))

           (read-decimal-as-inexact #false)
           (read-accept-dot         #false)
           (read-accept-infix-dot   #false)
           (read-accept-quasiquote  #false)
           (read-accept-bar-quote   #false)
           
           (current-eval (add-annotation (current-eval)))

           ; To activate custom indent hook.
           (port-count-lines-enabled #true)
           (port-count-lines! (current-output-port))
           
           (m5:configure-error-display settings)

           (namespace-attach-module drs-namespace scheme-test-module-name)
           (namespace-require       scheme-test-module-name)
           ; hack: communicate with test-engine via mutually-chosen name  test~object
           (namespace-set-variable-value! 'test~object (build-test-engine))
           (scheme-test-data (list (drracket:rep:current-rep)
                                   drs-eventspace
                                   test-display%))
           (test-execute tests-on?)
           (test-silence #false)
           (test-format
            (make-formatter
             (λ (v o) (render-value/format v settings o test:render-value/format:width))))))
        
        (super on-execute settings run-in-user-thread)

        (run-in-user-thread ; Set global-port-print-handler after superclass since super sets it too.
         (λ () (define our-print-setup-parameters (drracket:language:make-setup-printing-parameters))
           (global-port-print-handler
            (λ (value port [depth 0])
              (m5:render our-print-setup-parameters value settings port 'infinity))))))

      ; ●● Printing Values, for DrRacket languages. ●●

      ; Docs say: for user display, fit width via newlines and final newline. To tester above.
      (define/override (render-value/format v settings port max-width-characters)
        (m5:render drracket:language:setup-printing-parameters v settings port max-width-characters))
      (define/override (render-value v settings port)
        (m5:render drracket:language:setup-printing-parameters v settings port 'infinity))
      
      ; How our settings take effect: used in the two methods that everything ends up delegating to.
      (define/private (set-printing-parameters settings thunk)
        (define (print-types?) (and (compthink-lang-settings-prefix-types? settings)
                                    (not (suppress-type-prefix))))
        (compthink:set-printing-parameters print-types? thunk))

      ; ● Reference [7.4] 10.2 Exceptions / 10.2.4 Configuring Default Handling. The printing aspects:
      #;{{error-display-handler error-value->string-handler}
         {error-print-source-location error-print-width}}
      
      ; From  on-execute / run-in-user-thread  above.
      (define/private (m5:configure-error-display settings)
        (error-display-handler m5:error-display-handler) ; errortrace unit implements below
        (error-value->string-handler (λ (x y) (m5:error-value->string settings x y)))
        (error-print-source-location #false))

      ; For racket/base error-value->string-handler : string rep of value inside error message.
      ; Print our way, but without any whole-expression customization, and truncate.
      ; Is  len  from  error-print-width ?
      (define/private (m5:error-value->string settings v len)
        (parameterize ([suppress-type-prefix #true])
          (define sp (open-output-string))
          (set-printing-parameters settings (λ () (print v sp)))
          (flush-output sp) ; necessary?
          (truncate (get-output-string sp) len)))
      
      ; ● Main value rendering, for previous three methods and  global-print-handler  in  on-execute .
      ; Indirectly via  print  in  m5:error-value->string , not sure exactly what the indirection
      ;  accomplishes and even if harmful now. Not sure of relation with  m5:error-display-handler .
      (define/private (m5:render setup-printing-parameters value settings port width)
        (define value₁ (printable-value value))
        (setup-printing-parameters ;; set drracket's printing parameters
         (λ () (set-printing-parameters
                settings ;; then adjust the settings for the teaching languages
                (λ () (call-with-values
                       (λ () (drracket:language:simple-module-based-language-convert-value
                              value₁ settings))
                       (λ (converted-value [write? #true])
                         (define pretty-out (if write? pretty-write pretty-print))
                         (cond [(drracket:language:simple-settings-insert-newlines settings)
                                (if (number? width)
                                    (parameterize ([pretty-print-columns  width])
                                      (pretty-out converted-value port))
                                    (pretty-out converted-value port))]
                               [else (parameterize ([pretty-print-columns 'infinity])
                                       (pretty-out converted-value port))
                                     (newline port)]))))))
         settings width))

      (super-new)))

  (define (language-extension %)
    (class % (inherit get-module default-settings #;get-htdp-style-delta get-reader)
      (inherit-field reader-module)

      ; Alter capabilities: help context, and dock test menu.
      (define/augment (capability-value key)
        (case key
          [(drscheme:help-context-term) "L:csc104/2019-fall"]
          [(tests:test-menu tests:dock-menu) #true]
          [else (inner (drracket:language:get-capability-default key) capability-value key)]))
      
      ; Reading, inclduding metadata handling.
      (define/override (get-reader-module) reader-module)
      (define/override (get-metadata modname settings)
        (string-append (string-join htdp-save-file-prefix "\n" #:after-last "\n")
                       (format "#reader~s~s\n"
                               reader-module
                               `((modname ,modname)
                                 (compthink-settings ,(compthink-lang-settings->hash settings))))))
      (define/override (get-metadata-lines) 3)
      (define/override (metadata->settings metadata)
        (define (massage-metadata metadata)
          (if (and (list? metadata) (andmap (λ (x) (and (pair? x) (symbol? (car x)))) metadata))
              metadata
              '()))
        (define (metadata->table metadata)
          (with-handlers ([exn:fail:read? (λ (x) #false)])
            (define p (open-input-string metadata)) ; Skip to reader, skip module, then read metadata.
            (regexp-match #rx"\n#reader" p) (read p) (read p)))
        (define table (massage-metadata (metadata->table metadata)))
        (define s (assoc 'compthink-settings table))
        (if (and s (hash? (second s))) (hash->compthink-lang-settings (second s)) (default-settings)))

      ; Dummy program effect for a new tab that defaults csc104, or before run of opened program.
      ; Not sure how the wrapping occurs [test object, etc].
      (define/override (first-opened settings)
        (define-values (mod name) (create-empty-module (get-module)))
        (eval mod) (dynamic-require `',name #f) (current-namespace (module->namespace `',name))) 

      ; Interactions Header.
      (define the-style-delta (make-object style-delta%))
      (define/override (get-style-delta) the-style-delta)
      (define/override (extra-repl-information settings port)
        (parameterize ([current-output-port port])
          (display-extra-repl (drracket:rep:get-welcome-delta)
                              (drracket:rep:get-dark-green-delta)
                              (compthink-lang-settings-prefix-types? settings))))
        
      ; Read and wrap Definitions or Interactions code.
      ; • Definitions. Not affected by any of our settings.
      (define/override (front-end/complete-program port settings)
        #;(local-require "language/forms/styled-output.rkt")
        #;(announce-expanding)
        (expand-teaching-program port (get-reader) (get-module)))
      #;(define/override (front-end/finished-complete-program settings)
          (local-require "language/forms/styled-output.rkt")
          (announce-starting))
      ; • Interactions. HtDP wraps it with a reset of the test window and then a call to test,
      ;    because test reruns all recorded tests and appends the results.
      (define/override (front-end/interaction port settings)
        (define-values (t start? done?)
          (values (super front-end/interaction port settings) #true #false))
        (λ () (cond [start? (set! start? #false) #'(#%plain-app reset-tests)]
                    [done? eof]
                    [else (define ans (parameterize ([read-accept-lang #false]) (t)))
                          (cond [(eof-object? ans) (set! done? #true) #`(test)]
                                [else ans])])))
      (super-new)))

  ; ● errortrace/stacktrace ●

  (define (origin? v) (or (symbol? v) (path? v)))
  (define (errors-for-highlight ~exn)
    (ifs • (exn:srclocs? ~exn) ((exn:srclocs-accessor ~exn) ~exn) ; mimics highlight-errors/exn
         • (exn? ~exn) (map build-source-location (emfive-continuation-marks ~exn))
         #;{(define marks (emfive-continuation-marks ~exn))
            (if (null? marks) '() (build-source-location (car marks)))}
         • else '()))

  (define (♯string-split-once str sep)
    (define splits (string-split str sep #:trim? #false))
    (and (pair? splits) (list (first splits) (string-join (rest splits) sep))))
  (define (paired l)
    (define-values (1st remaining) (split-at l (min 2 (length l))))
    (if (empty? 1st) remaining (list* 1st (paired remaining))))
  (define (marked l) (apply append (for/list ([e l])
                                     (define c (styled-as-comment (first e)))
                                     (if (= (length e) 1)
                                         (list c)
                                         (list c (styled-as-symbol (second e)))))))
  (define (styled-as-symbol s) (list s token:symbol))
  
  (define (m5:error-display-handler _ ~exn)
    
    (test-silence #true)   

    (parameterize ([current-output-port (current-error-port)])
      (when (exn? ~exn) #;(display (exn→rewritten-message ~exn))
        (define message (exn→rewritten-message ~exn #:rich? #true))
        (define ♯who-what (♯string-split-once message ":"))
        (ifs • ♯who-what
             (define what (string-append " : " (string-trim (second ♯who-what))))
             (define what:parts (paired (string-split what "‸" #:trim? #false)))
             (display-styled-chunks (list* (styled-as-comment ";  ")
                                           (styled-as-symbol (string-trim (first ♯who-what)))
                                           (marked what:parts)))
             • else (display-styled-chunks (list (styled-as-comment ";" message))))
        (newline))
      (flush-output)) ; Delayed I/O could reset the error highlighting below.

    (define rep (drracket:rep:current-rep))
    (when (and (is-a? rep drracket:rep:text<%>) (eq? (send rep get-err-port) (current-error-port)))
      (define to-highlight (errors-for-highlight ~exn))
      (parameterize ([current-eventspace drs-eventspace])
        (queue-callback ;; Need to make sure user's eventspace still the same and running here?
         (λ₀ (send rep highlight-errors
                   (if #true #;(empty? to-highlight) to-highlight (list (first to-highlight)))
                   #;#false (reverse to-highlight)))))))
  
  ;  with-mark  ∈  stacktrace-imports^
  #;{syntax? syntax? exact-nonnegative-integer? → syntax?}
  ; Make marks suitable for  drracket:highlight-errors .
  (defines [(with-mark source-stx expr phase)
            (if (and (origin? (syntax-source source-stx))
                     (syntax-position source-stx)
                     (syntax-span source-stx))
                (with-syntax ([expr expr]
                              [mark (build-source-location-list source-stx)]
                              [emfive-continuation-mark-key emfive-continuation-mark-key]
                              [With-continuation-mark (shift phase #'with-continuation-mark)]
                              [Quote                  (shift phase #'quote)])
                  #'(With-continuation-mark (Quote emfive-continuation-mark-key) (Quote mark) expr))
                expr)]
    #:with
    [base-phase (variable-reference->module-base-phase (#%variable-reference))]
    [(shift phase stx) (syntax-shift-phase-level stx (- phase base-phase))])

  ; Disable profiling and test coverage (for now) from  stacktrace-imports^  from  stacktrace@ .
  (define-values (profiling-enabled
                  profile-key initialize-profile-point register-profile-start register-profile-done)
    (values (λ₀ #false) (gensym) void (λ_ #false) void))
  (define-values (test-coverage-enabled initialize-test-coverage-point test-covered)
    (values (make-parameter #false) void (λ_ #false)))
  
  (define-values/invoke-unit et:stacktrace@
    (import et:stacktrace-imports^) (export (prefix et: et:stacktrace^)))

  ;  wrap's current eval above
  (define ((add-annotation original-eval) expr)
    (original-eval (if (compiled-expression? (if (syntax? expr) (syntax-e expr) expr))
                       expr
                       (et:annotate-top (expand expr) (namespace-base-phase))))))
