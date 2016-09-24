#lang racket/base
(require parenlog)

(define-model family-tree
  (advisor barwise feferman)
  
  (:- (ancestor X Y)
      (advisor X Y)))

(query-model family-tree
             (frozzle feferman barwise))
(query-model family-tree
             (advisor barwise feferman))
(query-model family-tree
             (ancestor barwise Y))
