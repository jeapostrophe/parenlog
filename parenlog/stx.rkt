#lang racket/base
(module+ tests
  (require tests/eli-tester))

(define (variable-stx? q)
  (and (symbol? q)
       (char-upper-case?
        (string-ref
         (symbol->string q)
         0))))

(module+ tests
  (test
   (variable-stx? 'Foo)
   (variable-stx? 'FOO)
   (variable-stx? 'foo) => #f))

(provide variable-stx?)
