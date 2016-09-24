#lang racket/base
(module+ tests
  (require tests/eli-tester))

(define (variable? q)
  (and (symbol? q)
       (char-upper-case?
        (string-ref
         (symbol->string q)
         0))))

(module+ tests
  (test
   (variable? 'Foo)
   (variable? 'FOO)
   (variable? 'foo) => #f))

(provide variable?)
