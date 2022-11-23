#lang snack/pure (require snack/contract) (provide interactions-link-mixin)

#| Make language URLs in the REPL actually look like links.

 If the current language has a URL and a single style delta associated with it,
  color its name at the top of the interactions to look like a link.

 Not implemented as a drracket tool, must receive values of two bindings from such a tool. |#

(require (only-in racket/gui/base style-delta%)
         (only-in framework/gui-utils gui-utils:get-clickback-delta)

         (submod snack/class implement)

         (only-in racket/contract/region define/contract)

         (only-in snack/false let*/else))

(define (interactions-link-mixin drracket:rep:text<%>
                                 drracket:language-configuration:language-settings-language)
  (mixin (drracket:rep:text<%>) () (inherit get-definitions-text) (super-new)
    (define/override (reset-console)
      (let*/else
       ([definitions-text (get-definitions-text)]
        [language-settings (send definitions-text get-next-settings)]
        [language (drracket:language-configuration:language-settings-language language-settings)]
        [url (send language get-language-url)]
        [style-delta (send language get-style-delta)]
        [_ (is-a? style-delta style-delta%)]
        [#:else (super reset-console)])
       (define style-delta-copy (make-object style-delta%))
       (define clickback-delta (clickback-delta-copy))
       (send style-delta-copy copy style-delta)
       (send clickback-delta collapse style-delta)
       (send style-delta copy clickback-delta)
       (super reset-console)
       (send style-delta copy style-delta-copy)))))

(define/contract (clickback-delta-copy) (→ (is-a?/c style-delta%))
  (define copied (make-object style-delta%))
  (send copied copy (gui-utils:get-clickback-delta))
  copied)
