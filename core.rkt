#lang racket/base
(require racket/match
         racket/list
         racket/generator)
(module+ tests
  (require chk))

(define (id x) x)

(struct model (rules))
(struct var (d) #:prefab)
(struct query ())
(struct sexpr-query query (se) #:transparent)
(struct fun-query query (f args) #:transparent)
(struct fun-call query (f args anss) #:transparent)

(define (unbound-var? env q1)
  (and (var? q1)
       (not (hash-has-key? env q1))))
(module+ tests
  (chk
   #:t (unbound-var? #hasheq() (var 'Foo))
   #:t (unbound-var? #hasheq() (var 'FOO))
   #:! #:t (unbound-var? #hasheq() 'foo)
   #:! #:t (unbound-var? (hasheq (var 'Foo) 'X) 'Foo)
   #:! #:t (unbound-var? (hasheq (var 'Foo) 'X) (var 'Foo))
   #:! #:t (unbound-var? (hasheq (var 'Foo) 'X) (var 'FOO))
   #:! #:t (unbound-var? (hasheq (var 'Foo) 'X) 'foo)))

(define (bound-var? env q1)
  (and (var? q1)
       (hash-has-key? env q1)))
(module+ tests
  (chk
   #:! #:t (bound-var? #hasheq() (var 'Foo)) 
   #:! #:t (bound-var? #hasheq() (var 'FOO))
   #:! #:t (bound-var? #hasheq() 'foo) 
   #:! #:t (bound-var? (hasheq (var 'Foo) 'X) 'Foo) 
   #:t (bound-var? (hasheq (var 'Foo) 'X) (var 'Foo)) 
   #:! #:t (bound-var? (hasheq (var 'Foo) 'X) (var 'FOO)) 
   #:! #:t (bound-var? (hasheq (var 'Foo) 'X) 'foo)))

(define (unify env q1 q2)
  (cond
    [(equal? q1 q2)
     env]
    [(unbound-var? env q1)
     (hash-set env q1 q2)]
    [(unbound-var? env q2)
     (hash-set env q2 q1)]
    [(bound-var? env q1)
     (unify env (hash-ref env q1) q2)]
    [(bound-var? env q2)
     (unify env q1 (hash-ref env q2))]
    [(and (pair? q1) (pair? q2))
     (define new-env (unify env (car q1) (car q2)))
     (and new-env
          (unify new-env (cdr q1) (cdr q2)))]
    [else #f]))
(module+ tests
  (define var-X (var 'X))
  (define var-Y (var 'Y))
  (chk
   (unify #hasheq() 'foo 'foo) #hasheq()
   (unify #hasheq() 'foo 'bar) #f
   (unify #hasheq() var-X 'foo) (hasheq var-X 'foo)
   (unify #hasheq() 'foo var-X) (hasheq var-X 'foo)
   (unify #hasheq() var-X var-X) #hasheq()
   (unify #hasheq() var-X var-Y) (hasheq var-X var-Y)
   (unify #hasheq([Y . foo]) var-X var-Y) (hasheq var-Y 'foo
                                                  var-X var-Y)
   (unify #hasheq() '(s z) 's) #f
   (unify #hasheq() 's '(s z)) #f
   (unify #hasheq() '(s z) '(s z)) #hasheq()
   (unify #hasheq() (list 's var-X) '(s z)) (hasheq var-X 'z)
   (unify #hasheq([X . z]) (list 's var-X) '(s z)) (hasheq var-X 'z)
   (unify #hasheq([X . z]) (list 's var-X) '(s z z)) #f
   (unify #hasheq([X . z]) (list 's var-X 'z) '(s z)) #f

   (unify #hasheq()
          '(type Gamma (if Test Then Else) Tau)
          '(type mt (if boolConst numConst numConst) num))
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

(define (make-rule var-ids get-head&body)
  (lambda (model env query)
    (define vars (map (λ (v) (var (gensym v))) var-ids))
    (define-values (head-sans-vars body-sans-vars) (apply get-head&body vars))
    (define new-env (unify env head-sans-vars query))
    (generator
     ()
     (when new-env
       (reyield yield id (model-env-generator/queries model new-env body-sans-vars)))
     (yield query-model-generator-done))))

(define (rule-env-generator m r env q)
  (r m env q))

(define-syntax-rule (reyield yield f g)
  (for ([ans (in-producer g query-model-generator-done)])
    (yield (f ans))))

(define (model-env-generator/queries m env qs)
  (generator
   ()
   (match qs
     [(list) (yield env)]
     [(list-rest q1 qs)
      (for ([new-env (in-producer (model-env-generator m env q1)
                                  query-model-generator-done)])
        (reyield yield id (model-env-generator/queries m new-env qs)))])
   (yield query-model-generator-done)))

(define (model-env-generator m env first-query)
  (generator
   ()
   (match first-query
     [(struct sexpr-query (q-se))
      (for ([rule (in-list (model-rules m))])
        (reyield yield id (rule-env-generator m rule env q-se)))]
     [(struct fun-call (f args anss))
      (call-with-values
       (λ () (apply f (env-deref env args)))
       (λ vals
         (define new-env (unify env anss vals))
         (when new-env
           (yield new-env))))]
     [(struct fun-query (f args))
      (when (apply f (env-deref env args))
        (yield env))])
   (yield query-model-generator-done)))

(define (env-deref env v)
  (cond
    [(bound-var? env v)
     (env-deref env (hash-ref env v))]
    [(cons? v)
     (cons (env-deref env (car v))
           (env-deref env (cdr v)))]
    [else
     v]))

(define (env-restrict env l)
  (for/hasheq ([(k v) (in-hash env)]
               #:when (member k l))
    (values k (env-deref env v))))

(define (vars-in q)
  (match q
    [(? var? v) (list v)]
    [(struct fun-query (_ l))
     (append-map vars-in l)]
    [(struct fun-call (_ a r))
     (append (append-map vars-in a)
             (append-map vars-in r))]
    [(struct sexpr-query (l))
     (append-map vars-in l)]
    [(cons a d)
     (append (vars-in a) (vars-in d))]
    [x empty]))

(define (query-model-generator* m q)
  (define init-vars (vars-in q))
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
         var var-d
         fun-query fun-call sexpr-query
         make-rule
         query-model*
         query-model-generator*
         query-model-generator-done)
(module+ support
  (define (generator-map f g)
    (generator
     ()
     (reyield yield f g)
     (yield query-model-generator-done)))
  (provide generator-map
           model-rules))
