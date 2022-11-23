#lang snack/pure (require snack/contract)

(provide (contract-out (configuration:prompt-text
                        (and/c string? #;(property/c string-length (=/c 2))))))

; See also "menu-keys.rkt".

(define configuration:prompt-text "↪ ")
