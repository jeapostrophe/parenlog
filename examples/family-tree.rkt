#lang racket/base
(require racket/list
         parenlog)
(module+ test
  (require chk))

(define-model family-tree
  (advisor barwise feferman)
  (advisor feferman tarski)
  (advisor tarski lesniewski)
  (advisor lesniewski twardowski)
  (advisor twardowski brentano)
  (advisor brentano clemens)

  (:- (ancestor X Y)
      (advisor X Y))

  (:- (ancestor X Y)
      (advisor X Z)
      (ancestor Z Y))

  (advisor brentano trendelenburg)

  (:- (descendant X Y)
      (ancestor Y X))

  (advisee tarski montague)
  (advisee tarski mostowski)
  (advisee tarski robinson)

  (:- (advisor X Y)
      (advisee Y X))

  (:- (sibling/wrong X Y)
      (advisor X Z)
      (advisor Y Z))

  (:- (sibling X Y)
      (advisor X Z)
      (advisor Y Z)
      (,(compose not equal?) X Y)))

(module+ test
  (chk
   (query-model family-tree
                (frozzle feferman barwise))
   empty

   (query-model family-tree
                (advisor barwise feferman))
   (list #hasheq())

   (query-model family-tree
                (advisor feferman barwise))
   empty

   (query-model family-tree
                (ancestor barwise feferman))
   (list #hasheq())

   (query-model family-tree
                (ancestor barwise tarski))
   (list #hasheq())

   (query-model family-tree
                (advisor tarski barwise))
   empty

   (query-model family-tree
                (ancestor barwise clemens))
   (list #hasheq())

   (query-model family-tree
                (ancestor barwise trendelenburg))
   (list #hasheq())

   (query-model family-tree
                (descendant clemens montague))
   (list #hasheq())

   (query-model family-tree
                (descendant trendelenburg montague))
   (list #hasheq())

   (query-model family-tree
                (descendant feferman montague))
   empty

   (query-model family-tree
                (descendant barwise montague))
   empty

   (query-model family-tree
                (ancestor barwise X))
   (list #hasheq([X . feferman])
         #hasheq([X . tarski])
         #hasheq([X . lesniewski])
         #hasheq([X . twardowski])
         #hasheq([X . brentano])
         #hasheq([X . clemens])
         #hasheq([X . trendelenburg]))

   (query-model family-tree
                #:limit 6
                (ancestor X clemens))
   (list #hasheq([X . brentano])
         #hasheq([X . barwise])
         #hasheq([X . feferman])
         #hasheq([X . tarski])
         #hasheq([X . lesniewski])
         #hasheq([X . twardowski]))

   (query-model family-tree
                (descendant tarski X))
   (list #hasheq([X . feferman])
         #hasheq([X . montague])
         #hasheq([X . mostowski])
         #hasheq([X . robinson])
         #hasheq([X . barwise]))

   (query-model family-tree
                #:limit 4
                (ancestor X Y))
   (list #hasheq([X . barwise] [Y . feferman])
         #hasheq([X . feferman] [Y . tarski])
         #hasheq([X . tarski] [Y . lesniewski])
         #hasheq([X . lesniewski] [Y . twardowski]))

   (query-model family-tree
                (sibling/wrong robinson X))
   (list #hasheq([X . feferman])
         #hasheq([X . montague])
         #hasheq([X . mostowski])
         #hasheq([X . robinson]))

   (query-model family-tree
                (sibling robinson X))
   (list #hasheq([X . feferman])
         #hasheq([X . montague])
         #hasheq([X . mostowski]))))
