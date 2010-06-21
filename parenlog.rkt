#lang racket
(require (for-syntax syntax/parse)
         "core.rkt")

(define-syntax (:- stx)
  (raise-syntax-error ':- "Cannot be used outside define-model" stx))

(define-syntax (paren-stmt->rule stx)
  (syntax-parse
   stx
   #:literals (:-)
   [(_ (:- head body-query ...))
    (syntax/loc stx
      (compile-rule head
                    body-query ...))]
   [(_ head)
    (syntax/loc stx
      (paren-stmt->rule (:- head)))]))

(define-syntax (define-model stx)
  (syntax-parse
   stx
   [(_ model:id paren-stmt ...)
    (syntax/loc stx
      (define model
        (make-model
         (list (paren-stmt->rule paren-stmt)
               ...))))]))

(define-syntax (query-model stx)
  (syntax-parse 
   stx
   [(_ model:expr options ... query)
    (syntax/loc stx
      (query-model* model options ...
                    (compile-query query)))]))

(provide define-model
         query-model
         :-
         model?)