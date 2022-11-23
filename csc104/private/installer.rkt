#lang racket/base

(provide post-installer)
(define (post-installer . args)
  (newline)
  (displayln "•••••••••••••••••••••••••••••••••••")
  (displayln "•      Installation Complete.     •")
  (displayln "• Please click the \"Close\" button •")
  (displayln "•      and restart DrRacket.      •")
  (display   "•••••••••••••••••••••••••••••••••••"))
