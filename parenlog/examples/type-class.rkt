#lang racket/base
(require parenlog)

(define-model type-classes
  (show num show-Num)
  (show bool show-Bool)
  (:- (show (list X)
            (curry map Show-X))
      (show X Show-X))
  (:- (show (pair X Y)
            (show-Pair Show-X Show-Y))
      (show X Show-X)
      (show Y Show-Y)))

(query-model type-classes
             (show (pair num (list (pair num bool))) X))

(require parenlog/core)

