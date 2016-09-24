#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         "core.rkt")

(define-syntax (:- stx)
  (raise-syntax-error ':- "Cannot be used outside define-model" stx))

(define-syntax (paren-stmt->rule stx)
  (syntax-parse stx
    #:literals (:-)
    [(_ (:- head body-query ...))
     (syntax/loc stx
       (compile-rule head
                     body-query ...))]
    [(_ head)
     (syntax/loc stx
       (paren-stmt->rule (:- head)))]))

(define-syntax (define-model stx)
  (syntax-parse stx
    [(_ the-model:id paren-stmt ...)
     (syntax/loc stx
       (define the-model
         (model
          (list (paren-stmt->rule paren-stmt)
                ...))))]))

(define-syntax (compile-query stx)
  (syntax-parse stx
    #:literals (unquote)
    [(_ ((unquote f) arg ...))
     (syntax/loc stx
       (fun-query f (list 'arg ...)))]
    [(_ query)
     (syntax/loc stx
       (sexpr-query 'query))]))

(define-syntax (query-model stx)
  (syntax-parse stx
    [(_ model:expr options ... query)
     (syntax/loc stx
       (query-model*
        model options ...
        (compile-query query)))]))

(define-syntax-rule (query-model-generator m q)
  (query-model-generator* m (compile-query q)))

(provide model?
         query-model-generator-done
         define-model :-
         query-model
         query-model-generator)
