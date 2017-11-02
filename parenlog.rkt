#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     racket/list
                     "stx.rkt")
         "core.rkt"
         (submod "core.rkt" support))

(define-syntax (:- stx)
  (raise-syntax-error ':- "Cannot be used outside define-model" stx))

;; XXX use syntax-classes for these things
(begin-for-syntax
  (define (rewrite-se stx)
    (cond
      [(identifier? stx)
       (if (variable-stx? (syntax->datum stx))
         stx
         (quasisyntax/loc stx (quote #,stx)))]
      [(cons? stx)
       (quasisyntax
         (cons #,(rewrite-se (car stx)) #,(rewrite-se (cdr stx))))]
      [(syntax? stx)
       (rewrite-se (syntax-e stx))]
      [(empty? stx)
       #''()]
      [else
       stx]))

  (define (rewrite-body-query stx)
    (syntax-parse
        stx #:literals (unquote :- unsyntax)
        [((unquote f) arg ... :- ans ...)
         (quasisyntax/loc stx
           (fun-call f #,(rewrite-se #'(arg ...))
                     #,(rewrite-se #'(ans ...))))]
        [((unquote f) arg ...)
         (quasisyntax/loc stx
           (fun-query f #,(rewrite-se #'(arg ...))))]
        [(unsyntax se-expr)
         (quasisyntax/loc stx
           (sexpr-query se-expr))]
        [_
         (quasisyntax/loc stx
           (sexpr-query #,(rewrite-se stx)))]))

  (define (extract-se stx)
    (cond
      [(identifier? stx)
       (if (variable-stx? (syntax->datum stx))
         (list stx)
         empty)]
      [(cons? stx)
       (cons (extract-se (car stx)) (extract-se (cdr stx)))]
      [(syntax? stx)
       (extract-se (syntax-e stx))]
      [else
       empty]))

  (define (extract-body stx)
    (syntax-parse
        stx #:literals (unquote :- unsyntax)
        [((unquote f) arg ... :- ans ...)
         (extract-se #'(arg ... ans ...))]
        [((unquote f) arg ...)
         (extract-se #'(arg ...))]
        [(unsyntax expr)
         empty]
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
    [(_ the-model:id
        (~seq #:require required-mod:id) ...
        paren-stmt ...)
     (syntax/loc stx
       (define the-model
         (model
          (append
           (model-rules required-mod) ...
           (list (paren-stmt->rule paren-stmt)
                 ...)))))]))

(define-syntax (compile-query stx)
  (with-syntax ([(a-var ...) (flatten (extract-body stx))])
    (syntax-parse
        stx #:literals (unquote :- unsyntax)
        [(_ ((unquote f) arg ... :- ans ...))
         (syntax/loc stx
           (let ([a-var (var 'a-var)] ...)
             (fun-call f #,(rewrite-se #'(arg ...))
                       #,(rewrite-se #'(ans ...)))))]
        [(_ ((unquote f) arg ...))
         (syntax/loc stx
           (let ([a-var (var 'a-var)] ...)
             (fun-query f #,(rewrite-se #'(arg ...)))))]
        [(_ (unsyntax query-expr))
         (quasisyntax/loc stx
           (sexpr-query query-expr))]
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

;; XXX Add == and _
