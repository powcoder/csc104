#lang snack/pure (require "form-names.rkt")

(provide:big-bang)

(module keywords racket/base (provide emfive-keywords)
  (require "form-names.rkt")
  (define emfive-keywords (list* name:big-bang '{to-draw on-tick on-key on-mouse})))
