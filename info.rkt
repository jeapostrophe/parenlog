#lang setup/infotab
(define name "Parenlog")
(define blurb
  (list "A language that is similar to Prolog"))
(define scribblings '(["parenlog.scrbl" ()]))
(define categories '(devtools))
(define version "1.0")
(define primary-file "main.rkt")
(define compile-omit-paths '("tests" "examples"))
(define release-notes (list "Working with new Racket"))
(define repositories '("4.x"))
