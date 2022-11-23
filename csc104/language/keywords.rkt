#lang snack/pure ; Collect color lexer keywords from without-gui.rkt steps.rkt universe.rkt .

(require snack/contract)

(provide (contract-out [emfive-keywords (listof string?)]))

(require (only-in (submod "without-gui.rkt" keywords) [emfive-keywords keywords:without-gui])
         (only-in (submod "steps.rkt"       keywords) [emfive-keywords keywords:step])
         (only-in (submod "universe.rkt"    keywords) [emfive-keywords keywords:universe]))

(define emfive-keywords (map symbol->string (append keywords:without-gui
                                                    keywords:step
                                                    keywords:universe)))
