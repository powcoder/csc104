#lang snack/pure

(provide (struct-out /C))

(struct /C (predicate name) #:property prop:procedure (struct-field-index predicate))