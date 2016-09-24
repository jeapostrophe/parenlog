#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     racket/list
                     "stx.rkt")
         "core.rkt"
         (submod "core.rkt" support))

(define-syntax (:- stx)
  (raise-syntax-error ':- "Cannot be used outside define-model" stx))

(begin-for-syntax
  (define (rewrite-se stx)
    (cond
      [(identifier? stx)
       (if (variable-stx? (syntax->datum stx))
         stx
         (quasisyntax/loc stx (quote #,stx)))]
      [(syntax->list stx)
       => (lambda (stxs)
            (quasisyntax/loc stx
              (list #,@(map rewrite-se stxs))))]
      [else
       stx]))

  (define (rewrite-body-query stx)
    (syntax-case stx (unquote)
      [((unquote f) arg ...)
       (quasisyntax/loc stx
         (fun-query f #,(rewrite-se #'(arg ...))))]
      [_
       (quasisyntax/loc stx
         (sexpr-query #,(rewrite-se stx)))]))

  (define (extract-se stx)
    (cond
      [(identifier? stx)
       (if (variable-stx? (syntax->datum stx))
         (list stx)
         empty)]
      [(syntax->list stx)
       => (λ (x) (map extract-se x))]
      [else
       empty]))

  (define (extract-body stx)
    (syntax-case stx (unquote)
      [((unquote f) arg ...)
       (extract-se #'(arg ...))]
      [_
       (extract-se stx)]))

  (define (extract-vars head-stx body-stxs)
    (remove-duplicates
     (flatten
      (cons (extract-se head-stx)
            (map extract-body body-stxs)))
     #:key syntax-e)))

(define-syntax (paren-stmt->rule stx)
  (syntax-parse stx
    #:literals (:-)
    [(_ (:- head body-query ...))
     (with-syntax ([(a-var ...) (extract-vars #'head (syntax->list #'(body-query ...)))]
                   [head-sans-vars (rewrite-se #'head)]
                   [(body-query-sans-vars ...)
                    (map rewrite-body-query
                         (syntax->list #'(body-query ...)))])
       (syntax/loc stx
         (make-rule
          (λ ()
            ;; XXX Move this into make-rule
            (define a-var (var (gensym 'a-var))) ...
            (values head-sans-vars
                    (list body-query-sans-vars ...))))))]
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
  (with-syntax ([(a-var ...) (flatten (extract-body stx))])
    (syntax-parse stx
      #:literals (unquote)
      [(_ ((unquote f) arg ...))
       (syntax/loc stx
         (let ([a-var (var 'a-var)] ...)
           (fun-query f #,(rewrite-se #'(arg ...)))))]
      [(_ query)
       (quasisyntax/loc stx
         (let ([a-var (var 'a-var)] ...)
           (sexpr-query #,(rewrite-se #'query))))])))

(define (remove-vars ht)
  (for/hasheq ([(k v) (in-hash ht)])
    (values (var-d k) v)))

(define-syntax (query-model stx)
  (syntax-parse stx
    [(_ model:expr options ... query)
     (syntax/loc stx
       (map remove-vars
            (query-model*
             model options ...
             (compile-query query))))]))

(define-syntax-rule (query-model-generator m q)
  (generator-map remove-vars (query-model-generator* m (compile-query q))))

(provide model?
         query-model-generator-done
         define-model :-
         query-model
         query-model-generator)
