#lang racket/base (provide (rename-out [-read-syntax read-syntax]
                                       [-read read]))

(require racket/require (multi-in snack {syntax definition}))

(define configuration:lang 'csc104/2019-fall)
(define-macro (configuration:read-parameterization reading:expr ...+)
  (parameterize ([read-accept-lang #false]
                 [read-decimal-as-inexact #false]
                 [read-accept-dot #false])
    reading ...))

(defines
  [-read-syntax (make-read-syntax configuration:lang)]
  [-read        (make-read        configuration:lang)]
  #:with
  [((make-read language) [port (current-input-port)])
   (syntax->datum ((make-read-syntax language) 'whatever port))]  
  [((make-read-syntax language) [source-name #false] [port (current-input-port)]) 
   (local-require (only-in racket/list first)
                  (only-in racket/dict dict-ref)
                  (only-in racket/port port->list)
                  (only-in racket/function curry)) 
   (let* ([table (read port)]
          [path (object-name port)]
          [modname (if (or (path? path) ; Non-string path, for current platform.
                           (and (string? path) ; To avoid non-string path invalid for current platform.
                                (path-string? path)))
                       (let-values ([(base name dir) (split-path path)])
                         (string->symbol (path->string (path-replace-suffix name #""))))
                       (first (dict-ref table 'modname)))])
     (datum->syntax #false `(module ,modname ,language
                              (#%module-begin
                               ,@(configuration:read-parameterization
                                  (port->list (curry read-syntax source-name) port))))))])
