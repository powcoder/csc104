#lang snack/pure (provide emfive-continuation-marks
                          emfive-continuation-mark-key)

(define emfive-continuation-mark-key (gensym 'emfive-continuation-mark-key))

(define (emfive-continuation-marks exn)
  (continuation-mark-set->list (exn-continuation-marks exn) emfive-continuation-mark-key))
