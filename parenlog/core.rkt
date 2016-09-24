#lang racket/base
(require (for-syntax syntax/parse
                     racket/base
                     racket/list
                     "stx.rkt")
         racket/match
         racket/list
         "stx.rkt"
         racket/generator)
(module+ tests
  (require tests/eli-tester))

(struct model (rules))
(struct query ())
(struct sexpr-query query (se))
(struct fun-query query (f args))

(define (unbound-variable? env q1)
  (and (variable? q1)
       (not (hash-has-key? env q1))))
(module+ tests
  (test
   (unbound-variable? #hasheq() 'Foo)
   (unbound-variable? #hasheq() 'FOO)
   (unbound-variable? #hasheq() 'foo) => #f
   (unbound-variable? #hasheq([Foo . X]) 'Foo) => #f
   (unbound-variable? #hasheq([Foo . X]) 'FOO) => #t
   (unbound-variable? #hasheq([Foo . X]) 'foo) => #f))

(define (bound-variable? env q1)
  (and (variable? q1)
       (hash-has-key? env q1)))
(module+ tests
  (test
   (bound-variable? #hasheq() 'Foo) => #f
   (bound-variable? #hasheq() 'FOO) => #f
   (bound-variable? #hasheq() 'foo) => #f
   (bound-variable? #hasheq([Foo . X]) 'Foo) => #t
   (bound-variable? #hasheq([Foo . X]) 'FOO) => #f
   (bound-variable? #hasheq([Foo . X]) 'foo) => #f))

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
     (define new-env (unify env (car q1) (car q2)))
     (and new-env
          (unify new-env (cdr q1) (cdr q2)))]
    [else #f]))
(module+ tests
  (test
   (unify #hasheq() 'foo 'foo) => #hasheq()
   (unify #hasheq() 'foo 'bar) => #f
   (unify #hasheq() 'X 'foo) => #hasheq([X . foo])
   (unify #hasheq() 'foo 'X) => #hasheq([X . foo])
   (unify #hasheq() 'X 'X) => #hasheq()
   (unify #hasheq() 'X 'Y) => #hasheq([X . Y])
   (unify #hasheq([Y . foo]) 'X 'Y) => #hasheq([Y . foo] [X . Y])
   (unify #hasheq() '(s z) 's) => #f
   (unify #hasheq() 's '(s z)) => #f
   (unify #hasheq() '(s z) '(s z)) => #hasheq()
   (unify #hasheq() '(s X) '(s z)) => #hasheq([X . z])
   (unify #hasheq([X . z]) '(s X) '(s z)) => #hasheq([X . z])
   (unify #hasheq([X . z]) '(s X) '(s z z)) => #f
   (unify #hasheq([X . z]) '(s X z) '(s z)) => #f

   (unify #hasheq()
          '(type Gamma (if Test Then Else) Tau)
          '(type mt (if boolConst numConst numConst) num))
   =>
   #hasheq([Gamma . mt]
           [Test . boolConst]
           [Then . numConst]
           [Else . numConst]
           [Tau . num])

   (unify #hasheq((Gamma212327 . mt)
                  (Fun212328 . (fun x numConst))
                  (Arg212329 . boolConst)
                  (T2212330 . T)
                  (Gamma212347 . Gamma212327)
                  (Var212348 . x)
                  (Body212349 . numConst))
          '(T1212350 -> T2212351)
          '(T1212331 -> T2212330))
   =>
   #hasheq((Gamma212327 . mt)
           (Fun212328 . (fun x numConst))
           (Arg212329 . boolConst)
           (T2212330 . T)
           (T1212350 . T1212331)
           (T2212351 . T2212330)
           (Gamma212347 . Gamma212327)
           (Var212348 . x)
           (Body212349 . numConst))

   (unify #hasheq((X81793 . clemens) (Y81794 . montague)
                  (X81827 . Y81794) (Y81828 . X81793)
                  (X81837 . X81827) (Y81838 . Z81829))
          '(advisee tarski montague)
          '(advisee Y81838 X81837))))

(define query-model-generator-done
  (let ()
    (struct uniq ())
    (uniq)))

(begin-for-syntax
  (define (rewrite-se stx)
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
       (if (variable? (syntax->datum stx))
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
            ()
            (when new-env
              (define body-sans-vars (list body-query-sans-vars ...))
              (reyield yield (model-env-generator/queries model new-env body-sans-vars)))
            (yield query-model-generator-done)))))]))

(define (rule-env-generator m r env q)
  (r m env q))

(define-syntax-rule (reyield yield g)
  (for ([ans (in-producer g query-model-generator-done)])
    (yield ans)))

(define (model-env-generator/queries m env qs)
  (generator
   ()
   (match qs
     [(list) (yield env)]
     [(list-rest q1 qs)
      (for ([new-env (in-producer (model-env-generator m env q1)
                                  query-model-generator-done)])
        (reyield yield (model-env-generator/queries m new-env qs)))])
   (yield query-model-generator-done)))

(define (env-derefs env xs)
  (map (λ (x) (env-deref env x)) xs))

(define (model-env-generator m env first-query)
  (generator
   ()
   (match first-query
     [(struct sexpr-query (q-se))
      (for ([rule (in-list (model-rules m))])
        (reyield yield (rule-env-generator m rule env q-se)))]
     [(struct fun-query (f args))
      (when (apply f (env-derefs env args))
        (yield env))])
   (yield query-model-generator-done)))

(define (env-deref env v)
  (cond
    [(bound-variable? env v)
     (env-deref env (hash-ref env v))]
    [(list? v)
     (env-derefs env v)]
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
    [(? symbol?) empty]
    [(? list? l)
     (append-map variables-in l)]))

(define (query-model-generator* m q)
  (define init-vars (variables-in q))
  (generator
   ()
   (for ([ans (in-producer (model-env-generator m (hasheq) q)
                           query-model-generator-done)])
     (yield (env-restrict ans init-vars)))
   (yield query-model-generator-done)))

(define (query-model* m q #:limit [limit +inf.0])
  (for/list ([ans (in-producer (query-model-generator* m q)
                               query-model-generator-done)]
             [i (in-range limit)])
    ans))

(provide model model?
         fun-query sexpr-query
         compile-rule
         query-model*
         query-model-generator*
         query-model-generator-done)
