#lang scheme
(require (for-syntax syntax/parse
                     scheme/list
                     scheme/function
                     "stx.ss")
         "stx.ss"
         scheme/generator)

(define-struct model (rules))
(define-struct query ())
(define-struct (sexpr-query query) (se))
(define-struct (fun-query query) (f args))

(define (unbound-variable? env q1)
  (and (variable? q1)
       (not (hash-has-key? env q1))))

(define (bound-variable? env q1)
  (and (variable? q1)
       (hash-has-key? env q1)))

(define (unify env q1 q2)
  (cond
    [(equal? q1 q2)
     env]
    [(unbound-variable? env q1)
     (hash-set env q1 q2)]
    [(unbound-variable? env q2)
     (hash-set env q2 q1)]
    [(bound-variable? env q1)
     (unify env (hash-ref env q1) q2)]
    [(bound-variable? env q2)
     (unify env q1 (hash-ref env q2))]
    [(and (pair? q1) (pair? q2))
     (let ([new-env (unify env (car q1) (car q2))])
       (and new-env
            (unify new-env (cdr q1) (cdr q2))))]
    [else #f]))

(define generator-done
  (local [(define-struct uniq ())]
    (make-uniq)))

(define-for-syntax (rewrite-se stx)
  (cond
    [(identifier? stx)
     (if (variable? (syntax->datum stx))
         stx
         (quasisyntax/loc stx (quote #,stx)))]
    [(syntax->list stx)
     => (lambda (stxs)
          (quasisyntax/loc stx
            (list #,@(map rewrite-se stxs))))]
    [else
     stx]))

(define-for-syntax (rewrite-body-query stx)
  (syntax-case stx (unquote)
    [((unquote f) arg ...)
     (quasisyntax/loc stx
       (make-fun-query f #,(rewrite-se #'(arg ...))))]
    [_
     (quasisyntax/loc stx
       (make-sexpr-query #,(rewrite-se stx)))]))

(define-for-syntax (extract-se stx)
  (cond
    [(identifier? stx)
     (if (variable? (syntax->datum stx))
         (list stx)
         empty)]
    [(syntax->list stx)
     => (curry map extract-se)]
    [else
     empty]))

(define-for-syntax (extract-body stx)
  (syntax-case stx (unquote)
    [((unquote f) arg ...)
     (extract-se #'(arg ...))]
    [_
     (extract-se stx)]))

(define-for-syntax (extract-vars head-stx body-stxs)
  (remove-duplicates
   (flatten
    (cons (extract-se head-stx)
          (map extract-body body-stxs)))
   #:key syntax-e))

(define-syntax (compile-rule stx)
  (syntax-case stx ()
    [(_ head body-query ...)
     (with-syntax ([(var ...) (extract-vars #'head (syntax->list #'(body-query ...)))]
                   [head-sans-vars (rewrite-se #'head)]
                   [(body-query-sans-vars ...) 
                    (map rewrite-body-query
                         (syntax->list #'(body-query ...)))])
       (syntax/loc stx
         (lambda (model env query)
           (define var (gensym 'var))
           ...
           (define new-env (unify env head-sans-vars query))         
           (generator
            (when new-env
              (let ([body-sans-vars (list body-query-sans-vars ...)])
                (reyield yield (model-env-generator/queries model new-env body-sans-vars))))
            (yield generator-done)))))]))

(define (rule-env-generator m r env q)
  (r m env q))

(define-syntax-rule (reyield yield g)
  (for ([ans (in-producer g generator-done)])
    (yield ans)))

(define (model-env-generator/queries m env qs)
  (generator
   (match qs
     [(list) (yield env)]
     [(list-rest q1 qs)
      (for ([new-env (in-producer (model-env-generator m env q1) generator-done)])
        (reyield yield (model-env-generator/queries m new-env qs)))])
   (yield generator-done)))

(define (model-env-generator m env first-query)
  (generator
   (match first-query
     [(struct sexpr-query (q-se))
      (for ([rule (in-list (model-rules m))])
        (reyield yield (rule-env-generator m rule env q-se)))]
     [(struct fun-query (f args))
      (when (apply f (map (curry env-deref env) args))
        (yield env))])
   (yield generator-done)))

(define (env-deref env v)
  (cond
    [(bound-variable? env v)
     (env-deref env (hash-ref env v))]
    [(list? v)
     (map (curry env-deref env) v)]
    [else
     v]))

(define (env-restrict env l)
  (for/hasheq ([(k v) (in-hash env)]
               #:when (member k l))
    (values k (env-deref env v))))

(define (variables-in q)
  (match q
    [(? variable? v) (list v)]
    [(struct fun-query (_ l))
     (append-map variables-in l)]
    [(struct sexpr-query (l))
     (append-map variables-in l)]
    [_ empty]))

(define (query-answer-generator m q)
  (define init-vars (variables-in q))
  (generator
   (for ([ans (in-producer (model-env-generator m (make-immutable-hasheq empty) q)
                           generator-done)])
     (yield (env-restrict ans init-vars)))
   (yield generator-done)))

(define (query-model* m q #:limit [limit +inf.0])
  (for/list ([ans (in-producer (query-answer-generator m q) generator-done)]
             [i (in-range limit)])
    ans))

(define-syntax (compile-query stx)
  (syntax-parse
   stx #:literals (unquote)
   [(_ ((unquote f) arg ...))
    (syntax/loc stx
      (make-fun-query f (list 'arg ...)))]
   [(_ query)
    (syntax/loc stx
      (make-sexpr-query 'query))]))

(provide (all-defined-out))