#lang info
(define collection "parenlog")
(define deps '("base"))
(define build-deps '("eli-tester" "racket-doc" "scribble-lib"))

(define scribblings '(["parenlog.scrbl" (multi-page) (language)]))
