#lang snack/pure (provide more-definitions-or-interactions)

(require #;(only-in "log.rkt" log-compthink-error)
         (multi-in snack {control menu false})
         (only-in snack/drracket ♯view-menu
                  ♯definitions-or-interactions-item
                  definitions-or-interactions-item-hidden?))

(define ((more-definitions-or-interactions #:definitions? definitions?) editor event)
  (define (log-error:no-show/hide) (void)
    #;(log-compthink-error
       "couldn't find one of the show or hide, of Definitions or Interactions, menu items"))
  (define (log-error:no-view-menu) (void) #;(log-compthink-error "couldn't find the View menu"))
  (let*/else ; Needs to be looked up each time.
   ([view-menu (♯view-menu editor)] [#:else (log-error:no-view-menu)])
   (let*/else ([main-item (♯definitions-or-interactions-item
                           view-menu #:definitions? definitions?)]
               [#:else (log-error:no-show/hide)])
              (if (definitions-or-interactions-item-hidden? main-item)
                  (invoke-callback main-item event)
                  (let*/else ([the-other-item (♯definitions-or-interactions-item
                                               view-menu #:definitions? (not definitions?))]
                              [#:else (log-error:no-show/hide)])
                             (unless (definitions-or-interactions-item-hidden? the-other-item)
                               (invoke-callback the-other-item event)))))))
