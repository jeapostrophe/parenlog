#lang racket/base
(require parenlog/core
         parenlog/stx
         tests/eli-tester)

(test
 (variable? 'Foo)
 (variable? 'FOO)
 (variable? 'foo) => #f)

(test
 (unbound-variable? #hasheq() 'Foo)
 (unbound-variable? #hasheq() 'FOO)
 (unbound-variable? #hasheq() 'foo) => #f
 (unbound-variable? #hasheq([Foo . X]) 'Foo) => #f
 (unbound-variable? #hasheq([Foo . X]) 'FOO) => #t
 (unbound-variable? #hasheq([Foo . X]) 'foo) => #f)

(test
 (bound-variable? #hasheq() 'Foo) => #f
 (bound-variable? #hasheq() 'FOO) => #f
 (bound-variable? #hasheq() 'foo) => #f
 (bound-variable? #hasheq([Foo . X]) 'Foo) => #t
 (bound-variable? #hasheq([Foo . X]) 'FOO) => #f
 (bound-variable? #hasheq([Foo . X]) 'foo) => #f)

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
        
 
 (unify #hasheq((X81793 . clemens) (Y81794 . montague) (X81827 . Y81794) (Y81828 . X81793) (X81837 . X81827) (Y81838 . Z81829))
        '(advisee tarski montague)
        '(advisee Y81838 X81837)))
