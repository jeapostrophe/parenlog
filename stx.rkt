#lang racket
(define (variable? q)
  (and (symbol? q)
       (char-upper-case?
        (string-ref 
         (symbol->string q)
         0))))

(provide (all-defined-out))