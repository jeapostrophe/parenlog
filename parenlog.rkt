#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/id-set
                     racket/list
                     "stx.rkt")
         syntax/parse/define
         "core.rkt"
         (submod "core.rkt" support))

(define-syntax (:- stx)
  (raise-syntax-error ':- "Cannot be used outside define-model" stx))

(begin-for-syntax
  (define (free-id-set-union* sets)
    (for/fold ([x (immutable-free-id-set)]) ([y (in-list sets)])
      (free-id-set-union x y)))
  
  (define-syntax-class query-se
    #:description "parenlog query s-expr"
    #:attributes (vars compiled)
    [pattern x:id
             #:do [(define xid (syntax->datum #'x))]
             #:attr vars
             (if (variable-stx? xid)
               (immutable-free-id-set (list #'x))
               (immutable-free-id-set))
             #:with compiled
             (if (variable-stx? xid)
               #'x
               (syntax/loc this-syntax
                 'x))]
    [pattern (a:query-se . d:query-se)
             #:attr vars (free-id-set-union (attribute a.vars) (attribute d.vars))
             #:with compiled #'(cons a.compiled d.compiled)]
    [pattern () #:attr vars (immutable-free-id-set) #:with compiled #''()])
  
  (define-syntax-class query-form
    #:description "parenlog query form"
    #:attributes (vars compiled)
    #:literals (unquote :- unsyntax)
    [pattern ((unquote f:expr) arg:query-se ... :- ans:query-se ...)
             #:attr vars (free-id-set-union* (attribute arg.vars))
             #:with compiled
             (syntax/loc this-syntax
               (fun-call f (list arg.compiled ...)
                          (list ans.compiled ...)))]
    [pattern ((unquote f:expr) arg:query-se ...)
             #:attr vars (free-id-set-union* (attribute arg.vars))
             #:with compiled
             (syntax/loc this-syntax
               (fun-query f (list arg.compiled ...)))]
    [pattern (unsyntax dyn-q:expr)
             #:attr vars (immutable-free-id-set)
             #:with compiled
             (syntax/loc this-syntax
               (sexpr-query dyn-q))]
    [pattern static-q:query-se
             #:attr vars (attribute static-q.vars)
             #:with compiled
             (syntax/loc this-syntax
               (sexpr-query static-q.compiled))])
  
  (define-syntax-class query
    #:description "parenlog query"
    #:attributes (compiled)
    [pattern q:query-form
             #:with (a-var ...) (free-id-set->list (attribute q.vars))
             #:with compiled
             (syntax/loc this-syntax
               (let ([a-var (var 'a-var)] ...)
                 q.compiled))])

  (define-syntax-class stmt
    #:description "parenlog statement"
    #:attributes (rule)
    #:literals (:-)
    [pattern (:- head:query-se body:query-form ...)
             #:with (a-var ...)
             (free-id-set->list
              (free-id-set-union*
               (cons (attribute head.vars)
                     (attribute body.vars))))
             #:attr rule
             (syntax/loc this-syntax
               (make-rule
                '(a-var ...)
                (λ (a-var ...)
                  (values head.compiled
                          (list body.compiled ...)))))]
    [pattern head:query-se
             #:with inner:stmt (syntax/loc this-syntax (:- head))
             #:attr rule (attribute inner.rule)]))

(define-syntax (define-model stx)
  (syntax-parse stx
    [(_ the-model:id
        (~seq #:require required-mod:id) ...
        ps:stmt ...)
     (syntax/loc stx
       (define the-model
         (model
          (append
           (model-rules required-mod) ...
           (list ps.rule ...)))))]))

(define (remove-vars ht)
  (for/hasheq ([(k v) (in-hash ht)])
    (values (var-d k) v)))

(define-syntax (query-model stx)
  (syntax-parse stx
    [(_ model:expr (~optional (~seq #:limit limit:expr)) q:query)
     #:with opts (if (attribute limit)
                   #'(#:limit limit)
                   #'())
     (syntax/loc stx
       (map remove-vars (query-model* model q.compiled . opts)))]))

(provide model?
         define-model :-
         query-model)

(module+ support
  (define-simple-macro (query-model-generator m:expr q:query)
    (generator-map remove-vars (query-model-generator* m q.compiled)))
  (provide query-model-generator-done
           query-model-generator))

;; XXX Add == and _
