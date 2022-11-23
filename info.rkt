#lang info

(define collection 'multi)

(define deps (list (list "base" '#:version "7.3")
                   "tightlight"
                   "snack"
                   "draw-lib"
                   "drracket-plugin-lib"
                   "errortrace-lib"
                   "gui-lib"
                   "htdp-lib"
                   "images-lib"
                   "net-lib"
                   "reprovide-lang"
                   #;"sandbox-lib"
                   #;"scribble-lib"
                   "snip-lib"
                   "option-contract-lib"
                   "parser-tools-lib"
                   "syntax-color-lib"))

(define compile-omit-paths (list "notes"))

(define build-deps (list #;"draw-doc"
                         #;"picturing-programs"
                         #;"scribble-text-lib"))
