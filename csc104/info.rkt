#lang info

(define collection "csc104")

(define name "CSC 104")

(define version "0.0.14")

(define drracket-tools (list (list "langs.rkt")))
(define drracket-tool-icons (list #false))
(define drracket-tool-names (list "CSC 104"))
(define drracket-tool-urls (list "http://www.cs.toronto.edu/~csc104h")) ; What should this be?

#;(define scribblings `(("scribblings/docs.scrbl"
                         ()
                         (teaching ,(if (getenv "PLT_PKG_BUILD_SERVICE") -inf.0 10)) "csc104")))

(define post-install-collection "private/installer.rkt")
