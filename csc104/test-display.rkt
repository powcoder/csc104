#lang racket/base

(provide test-panel% test-window% test-display% test-silence)

(module* langs #false
  (provide test-display% test-silence
           (all-from-out test-engine/racket-gui)
           (all-from-out test-engine/racket-tests))
  (require (only-in test-engine/racket-gui make-formatter)
           (only-in test-engine/racket-tests
                    test reset-tests scheme-test-data test-format test-execute build-test-engine)))

(require (only-in "shared-settings.rkt" suppress-type-prefix)
         "rewrite-error.rkt"
         (lib "test-engine/test-info.scm")
         test-engine/test-engine test-engine/print
         mred framework
         snack/string-constants
         racket/class racket/match)

(define test-display%
  (class* object% ()
    (init-field (current-rep #f))

    (define test-info #f)
    (define/pubment (install-info t) 
      (set! test-info t)
      (inner (void) install-info t))

    (define current-tab #f)
    (define drscheme-frame #f)
    (define src-editor #f)
    (define/public (display-settings df ct ed)
      (set! current-tab ct)
      (set! drscheme-frame df)
      (set! src-editor ed))

    (define (docked?)
      (and drscheme-frame
           (preferences:get 'test-engine:test-window:docked?)))
    
    (define/public (report-success)
      (when current-rep
        (unless current-tab
          (set! current-tab (send (send current-rep get-definitions-text) get-tab)))
        (unless drscheme-frame
          (set! drscheme-frame (send current-rep get-top-level-window)))
        (let ([curr-win (and current-tab (send current-tab get-test-window))])
          (when curr-win
            (let ([content (make-object (editor:standard-style-list-mixin text%))])
              (send content lock #t)
              (when curr-win (send curr-win update-editor content))
              (when current-tab (send current-tab current-test-editor content))
              (when (docked?)
                (send drscheme-frame display-test-panel content)
                (send curr-win show #f)))))))

    (define/public (display-success-summary port count)
      (unless (test-silence)
        (display (case count
                   [(0) (string-constant test-engine-0-tests-passed)]
                   [(1) (string-constant test-engine-1-test-passed)]
                   [(2) (string-constant test-engine-both-tests-passed)]
                   [else (format (string-constant test-engine-all-n-tests-passed)
                                 count)])
                 port)))

    (define/public (display-untested-summary port)
      (unless (test-silence)
        (display (string-constant test-engine-should-be-tested) port)
        (display "\n" port)))

    (define/public (display-disabled-summary port)
      (display (string-constant test-engine-tests-disabled) port)
      (display "\n" port))
    
    (define/public (display-results)
      (let* ([curr-win (and current-tab (send current-tab get-test-window))]
             [window (or curr-win (make-object test-window%))]
             [content (make-object (editor:standard-style-list-mixin text%))])
        
        (send this insert-test-results content test-info src-editor)
        (send content lock #t)
        (send window update-editor content)
        (when current-tab
          (send current-tab current-test-editor content)
          (unless curr-win
            (send current-tab current-test-window window)
            (send drscheme-frame register-test-window window)
            (send window update-switch (λ () (send drscheme-frame dock-tests)))
            (send window update-disable (λ () (send current-tab update-test-preference #f)))
            (send window update-closer (λ ()
                                         (send drscheme-frame deregister-test-window window)
                                         (send current-tab current-test-window #f)
                                         (send current-tab current-test-editor #f)))))
        (if (docked?)
            (send drscheme-frame display-test-panel content)
            (send window show #t))))

    (define/pubment (insert-test-results editor test-info src-editor)
      (let* ([style (send test-info test-style)]
             [total-tests (send test-info tests-run)]
             [failed-tests (send test-info tests-failed)]
             [total-checks (send test-info checks-run)]
             [failed-checks (send test-info checks-failed)]

             [check-outcomes
              (λ (total failed zero-message ck?)
                (send editor insert
                      (cond [(zero? total) zero-message]
                            [(= 1 total) (string-append
                                          (if ck?
                                              (string-constant test-engine-ran-1-check)
                                              (string-constant test-engine-ran-1-test))
                                          "\n")]
                            [else (format (string-append
                                           (if ck?
                                               (string-constant test-engine-ran-n-checks)
                                               (string-constant test-engine-ran-n-tests))
                                           "\n")
                                          total)]))
                (when (> total 0)
                  (send editor insert
                        (cond
                          [(and (zero? failed) (= 1 total))
                           (string-append (if ck?
                                              (string-constant test-engine-1-check-passed)
                                              (string-constant test-engine-1-test-passed))
                                          "\n\n")]
                          [(zero? failed) (string-append
                                           (if ck?
                                               (string-constant test-engine-all-checks-passed)
                                               (string-constant test-engine-all-tests-passed))
                                           "\n\n")]
                          [(= failed total) (string-append
                                             (if ck?
                                                 (string-constant test-engine-0-checks-passed)
                                                 (string-constant test-engine-0-tests-passed))
                                             "\n")]
                          [else (format (string-append
                                         (if ck?
                                             (string-constant test-engine-m-of-n-checks-failed)
                                             (string-constant test-engine-m-of-n-tests-failed))
                                         "\n\n")
                                        failed total)]))))]

             [check-outcomes/check
              (λ (zero-message)
                (check-outcomes total-checks failed-checks
                                zero-message #t))]
             [check-outcomes/test
              (λ (zero-message)
                (check-outcomes total-checks failed-checks
                                zero-message #f))]
             [test-outcomes 
              (λ (zero-message)
                (check-outcomes total-tests failed-tests
                                zero-message #f))])
        (case style
          [(test-require) (test-outcomes
                           (string-append (string-constant test-engine-must-be-tested) "\n"))
                          (check-outcomes/check
                           (string-append (string-constant test-engine-is-unchecked) "\n"))]
          [(check-require) (check-outcomes/check
                            (string-append (string-constant test-engine-is-unchecked) "\n"))]
          [(test-basic) (test-outcomes "")
                        (check-outcomes/check "")]
          [(test-check) (check-outcomes/test
                         (string-append (string-constant test-engine-must-be-tested)
                                        "\n"))]
          [else (check-outcomes/check "")])

        (unless (zero? total-checks)
          (inner (display-check-failures (send test-info failed-checks) editor test-info src-editor)
                 insert-test-results editor test-info src-editor))))
    
    (define/public (display-check-failures checks editor test-info src-editor)
      (when (pair? checks)
        (send editor insert (string-append (string-constant test-engine-check-failures) "\n")))
      (for ([failed-check (reverse checks)])
        (send editor insert "\n  ")
        (if (failed-check-exn? failed-check)
            (make-error-link editor
                             (failed-check-reason failed-check)
                             (failed-check-exn? failed-check)
                             (check-fail-src (failed-check-reason failed-check))
                             src-editor)
            (make-link editor
                       (failed-check-reason failed-check)
                       (check-fail-src (failed-check-reason failed-check))
                       src-editor))
        (send editor insert "\n")))

    ;next-line: editor% -> void
    ;Inserts a newline and a tab into editor
    (define/public (next-line editor) (send editor insert "\n\t"))

    ;; make-link: text% check-fail src editor -> void
    (define (make-link text reason dest src-editor)
      (display-reason text reason)
      (send text insert (make-string 4 #\space))
      (let ((start (send text get-end-position)))
        (send text insert (format-src dest))
        (when (and src-editor current-rep)
          (send text set-clickback
                start (send text get-end-position)
                (λ (t s e) (highlight-check-error dest src-editor))
                #f #f)
          (set-clickback-style text start "royalblue"))))

    (define (display-reason text fail)
      #;(write (list 'display-reason fail (check-fail? fail) (message-error? fail))
               (current-error-port))
      #;(newline (current-error-port))

      (parameterize ([suppress-type-prefix #t])
        (let* ([print-string (λ (m) (send text insert m))]
               [print-formatted (λ (v)
                                  (define cff (check-fail-format fail))
                                  (cond
                                    [(procedure-arity-includes? cff 2)
                                     (cff v (open-output-text-editor text))]
                                    [else
                                     (define m (cff v))
                                     (when (is-a? m snip%)
                                       (send m set-style (send (send text get-style-list)
                                                               find-named-style "Standard")))
                                     (send text insert m)]))]
               [the-printer (λ (fstring . vals)
                              (apply print-with-values fstring print-string print-formatted vals))]
               [formatter values])
          (match fail
            [(unexpected-error _ _ expected _ exn)
             (the-printer "check-expect encountered the following error, ~n     ")
             ; Can't use the given message field, because that was rewritten with the internal
             ; exception formatter and not ours
             (define error-msg (make-object string-snip% (exn→rewritten-message exn)))
             (send error-msg set-style
                   (send (editor:get-standard-style-list)
                         find-named-style "text:ports err"))
             (send text insert error-msg)

             ; Need to manually reset the style, otherwise the following text will also be red.
             (define reset-style (make-object string-snip% "\n"))
             (send reset-style set-style
                   (send (send text get-style-list)
                         find-named-style "Standard"))
             (send text insert reset-style)

             (the-printer "~n  instead of ~F, the expected value." expected)]
            [(unequal _ _ test actual)
             (the-printer (string-constant test-engine-actual-value-differs-error)
                          test actual)]
            [(message-error _ _ strings) (for-each print-formatted strings)])
          (print-string "\n"))))
    
    ;; make-error-link: text% check-fail exn src editor -> void
    (define (make-error-link text reason exn dest src-editor)
      (make-link text reason dest src-editor))

    (define (insert-messages text msgs)
      (for ([m msgs])
        (when (is-a? m snip%)
          (send m set-style (send (send text get-style-list)
                                  find-named-style "Standard")))
        (send text insert m)))

    (define (format-clickable-syntax-src text stx src-editor)
      (let ((start (send text get-end-position)))
        (send text insert (format-syntax-src stx))
        (send text set-clickback
              start (send text get-end-position)
              (λ (t s e) (highlight-error/syntax stx src-editor))
              #f #f)
        (set-clickback-style text start "blue")))

    (define (set-clickback-style text start color)
      (let ([end (send text get-end-position)]
            [c (new style-delta%)])
        (send text insert " ")
        (send text change-style
              (make-object style-delta% 'change-underline #t)
              start end #f)
        (send c set-delta-foreground color)
        (send text change-style c start end #f)))

    (define (format-syntax-src stx)
      (format-position (syntax-source stx)
                       (syntax-line stx) (syntax-column stx)))

    ;format-src: src -> string
    (define (format-src src) (format-position (car src) (cadr src) (caddr src)))

    (define (format-position file line column)
      (let ([line (cond [line => number->string]
                        [else (string-constant test-engine-unknown)])]
            [col (cond [column => number->string]
                       [else (string-constant test-engine-unknown)])])
          
        (if (path? file)
            (let-values ([(base name must-be-dir?)
                          (split-path file)])
              (if (path? name)
                  (format (string-constant test-engine-in-at-line-column)
                          (path->string name) line col)
                  (format (string-constant test-engine-at-line-column)
                          line col)))
            (format (string-constant test-engine-at-line-column)
                    line col))))

    (define (highlight-error source line column position span src-editor)
      (when (and current-rep src-editor)
        (cond
          [(is-a? src-editor text:basic<%>)
           (let ((highlight
                  (λ ()
                    (let ((error-src (if (send src-editor port-name-matches? source)
                                         ; definitions or REPL?
                                         src-editor
                                         current-rep)))
                      (send current-rep highlight-errors
                            (list (make-srcloc error-src line column position span))
                            #f)
                      (let ([frame (send current-tab get-frame)])
                        (unless (send current-tab is-current-tab?)
                          (let loop ([tabs (send frame get-tabs)] [i 0])
                            (unless (null? tabs)
                              (if (eq? (car tabs) current-tab)
                                  (send frame change-to-nth-tab i)
                                  (loop (cdr tabs) (add1 i))))))
                        (send frame show #t))))))
             (queue-callback highlight))])))

    (define (highlight-check-error srcloc src-editor)
      (let* ([src-pos cadddr]
             [src-span (λ (l) (car (cddddr l)))]
             [position (src-pos srcloc)]
             [span (src-span srcloc)])
        (highlight-error (car srcloc) (cadr srcloc) (caddr srcloc)
                         position span
                         src-editor))) 

    (define (highlight-error/syntax stx src-editor)
      (highlight-error (syntax-source stx) (syntax-line stx) (syntax-column stx)
                       (syntax-position stx) (syntax-span stx)
                       src-editor))

    (super-instantiate ())))

(frame:setup-size-pref 'compthink:test-engine-window-size 600 525
                       #:position-preferences 'compthink:test-engine-window-position)

(define test-window%
  (class* (frame:size-pref-mixin frame:standard-menus%) ()

    (super-new
     [label (string-constant test-engine-window-title)]
     [size-preferences-key 'compthink:test-engine-window-size]
     [position-preferences-key 'compthink:test-engine-window-position])

    (define switch-func void)
    (define disable-func void)
    (define close-cleanup void)

    (inherit get-area-container)
    
    (define content
      (make-object editor-canvas% (get-area-container) #f '(auto-vscroll)))

    (define button-panel
      (make-object horizontal-panel% (get-area-container)
        '() #t 0 0 0 0 '(right bottom) 0 0 #t #f))

    (define buttons
      (list (make-object button%
              (string-constant close)
              button-panel
              (λ (b c) (when (eq? 'button (send c get-event-type))
                         (close-cleanup)
                         (send this show #f))))
            (make-object button%
              (string-constant dock)
              button-panel
              (λ (b c) (when (eq? 'button (send c get-event-type))
                         (send this show #f)
                         (preferences:set 'test-engine:test-window:docked? #t)
                         (switch-func))))
            (make-object grow-box-spacer-pane% button-panel)))

    (define/override (edit-menu:between-select-all-and-find menu) (void))
    
    (define/public (update-editor e) (send content set-editor e))

    (define/public (update-switch thunk) (set! switch-func thunk))
    (define/public (update-closer thunk) (set! close-cleanup thunk))
    (define/public (update-disable thunk) (set! disable-func thunk))))

(define test-panel%
  (class* vertical-panel% ()
    (inherit get-parent)

    (super-instantiate ())

    (define content (make-object editor-canvas% this #f '()))
    (define button-panel (make-object horizontal-panel% this
                           '() #t 0 0 0 0 '(right bottom) 0 0 #t #f))
    (define (hide)
      (let ([current-tab (send frame get-current-tab)])
        (send frame deregister-test-window 
              (send current-tab get-test-window))
        (send current-tab current-test-window #f)
        (send current-tab current-test-editor #f))
      (remove))

    (make-object button%
      (string-constant hide)
      button-panel
      (λ (b c) (when (eq? 'button (send c get-event-type))
                 (hide))))
    (make-object button%
      (string-constant undock)
      button-panel
      (λ (b c) (when (eq? 'button (send c get-event-type))
                 (preferences:set 'test-engine:test-window:docked? #f)
                 (send frame undock-tests))))

    (define/public (update-editor e) (send content set-editor e))

    (define frame #f)
    (define/public (update-frame f) (set! frame f))

    (define/public (remove)
      (let ([parent (get-parent)])
        (preferences:set 'test-engine:test-dock-size (send parent get-percentages))
        (send parent delete-child this)))))
