#lang racket/base
(require racket/list
         racket/match
         parenlog)
(module+ test
  (require chk))

(define-model list-theory
  (append () Y Y)
  (:- (append (X . Y) Z (X . YZ))
      (append Y Z YZ))

  (:- (racket-append X Y Z)
      (,append X Y :- Z)))
(module+ test
  (chk (query-model list-theory (racket-append (a b) (c d) Z))
       (list (hasheq 'Z '(a b c d)))
       (query-model list-theory (append (a b) (c d) Z))
       (list (hasheq 'Z '(a b c d)))
       (query-model list-theory (append X (c d) (a b c d)))
       (list (hasheq 'X '(a b)))
       (query-model list-theory (append (a b) Y (a b c d)))
       (list (hasheq 'Y '(c d)))

       (query-model list-theory (append A B (a b)))
       (list (hasheq 'A '() 'B '(a b))
             (hasheq 'A '(a) 'B '(b))
             (hasheq 'A '(a b) 'B '()))))

(define-model linear-logic
  #:require list-theory
  
  (proves (A) A (assume A))

  ;; These rules force assumption lists to be non-empty, which removes
  ;; divergence. There should be a better way, not sure what it is
  ;; though.
  (:- (not-empty? L)
      (== L (_ . _)))
  
  (:- (proves Eta B (lolli-elim A B Pl Pa))
      (not-empty? Gamma)
      (not-empty? Delta)
      (append Gamma Delta Eta)
      (proves Gamma (lolli A B) Pl)
      (proves Delta A Pa))

  (:- (proves Eta (tensor A B) (tensor-intro A B Pa Pb))
      (not-empty? Gamma)
      (not-empty? Delta)
      (append Gamma Delta Eta)
      (proves Gamma A Pa)
      (proves Delta B Pb))

  (:- (provable? Gamma P)
      (proves Gamma P Pr)))

(module+ test
  (define convert-prop
    (match-lambda
      [(? symbol? a) (list 'const a)]
      [(list lhs '⊗ rhs) (list 'tensor (convert-prop lhs) (convert-prop rhs))]
      [(list lhs '⊸ rhs) (list 'lolli (convert-prop lhs) (convert-prop rhs))]))
  (define (convert-provable? p)
    (match-define (list a ... '⊧ goal) p)
    (list 'provable? (map convert-prop a) (convert-prop goal)))
  
  (define-syntax-rule (chk-ll prop ...)
    (begin (chk (query-model linear-logic #:limit 1 #,(convert-provable? 'prop))
                (list (hasheq)))
           ...))
  (chk-ll (a ⊧ a)
          (b ⊧ b)
          (a b ⊧ (a ⊗ b))
          ((a ⊸ b) a ⊧ b)
          ((b ⊸ c) (a ⊸ b) a ⊧ c)
          ((a ⊸ b) a c ⊧ (b ⊗ c))))
