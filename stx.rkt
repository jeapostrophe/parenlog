#lang racket/base
(module+ tests
  (require chk))

(define (variable-stx? q)
  (and (symbol? q)
       (char-upper-case?
        (string-ref
         (symbol->string q)
         0))))

(module+ tests
  (chk
   #:t (variable-stx? 'Foo)
   #:t (variable-stx? 'FOO)
   #:! #:t (variable-stx? 'foo)))

(provide variable-stx?)
