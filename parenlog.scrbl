#lang scribble/doc
@(require scribble/manual
          scribble/eval
          scribble/basic
          scribble/bnf
          (for-label racket/base
                     syntax/parse
                     racket/function
                     racket/contract
                     "main.rkt"))

@(define the-eval
   (let ([the-eval (make-base-eval)])
     (the-eval `(require parenlog))
     the-eval))

@title[#:tag "top"]{Parenlog}
@author[(author+email "Jay McCarthy" "jay@racket-lang.org")]

This package contains an implementation of a language very similar to pure @link["http://en.wikipedia.org/wiki/Prolog"]{Prolog}, except with parenthetical notation.

@section{Embedded Parenlog}

The easiest way to get started using Parenlog for Racket is with the main module:

@defmodule[parenlog]

Here is a basic example of using Parenlog:

@examples[#:eval the-eval
          (define-model family-tree
            (parent rogue moria)
            (parent rogue larn)
            (parent rogue omega)
            (parent rogue hack)
            (parent moria angband)
            (parent hack nethack)
            (parent angband tome)
            (parent angband zangband)
            (parent omega adom)
            (parent nethack adom)
            (parent nethack zapm)
            (parent nethack slashem)
            (parent nethack crawl)
            
            (:- (sibling X Y)
                (parent Z X)
                (parent Z Y)
                (,(compose not equal?) X Y))

            (:- (adds? X Y Z)
                (,+ X Y :- Z)))
          (query-model family-tree
                       (sibling adom zapm))
          (query-model family-tree
                       #:limit 4
                       (sibling X Y))
          (query-model family-tree
                       (adds? 5 6 Z))]

@defform/subs[#:literals (:- unquote ~seq ==)
              (define-model id (~seq #:require model-id) ... stmt ...)
              ([stmt head-query
                     (:- head-query body-query ...)]
               [head-query s-expr]
               [body-query s-expr
                           (== s-expr s-expr)
                           (,pred s-expr ...)
                           (,fun s-expr ... :- s-expr ...)])
              #:contracts
              ([id identifier?]
               [pred (any/c ... -> boolean?)]
               [fun (any/c ... -> any)])]{
Defines @racket[id] as a Parenlog model, extending each @racket[model-id].

@racket[_] counts as variable that is unique at every occurrence.

A @racket[_s-expr] may contain @racket[unsyntax] to escape to Racket.
}
                                              
@defidform[:-]{
 Syntax that may only appear within @racket[define-model].
}

@defform/subs[(query-model model-expr maybe-limit body-query)
              ([maybe-limit code:blank
                            (code:line #:limit limit-expr)])
              #:contracts
              ([model-expr model?]
               [limit-expr number?])]{
 Queries @racket[model-expr] with @racket[body-query] until
@racket[limit-expr] results are found or no results remain.
 
 Returns a value matching the contract: @racket[(listof (hash/c
symbol? anc/c))]. Each value in this list is a substitution of
@racket[body-query] that @racket[model-expr] proves.
}
                                     
@defproc[(model? [v any/c]) boolean?]{
 Returns @racket[#t] if @racket[v] was bound by @racket[define-model];
@racket[#f] otherwise.
}

@section{Standalone Parenlog}

@defmodulelang[parenlog]

Parenlog can also be used as a standalone module language.

At a high level, the body of a Parenlog module is the body of a
@racket[define-model] form and any REPL interaction is placed within a
@racket[query-model] form. There are, of course, a few caveats.

First, occurrences of @racket[(? body-query)] in the module body are
delayed and used as queries.

Second, all query results are printed. @litchar{no} is printed when
there are no answeres. @litchar{yes} is printed when there is an empty
substitution as an answer. Otherwise, the substitution is printed
using @racket[(hash-for-each _subst (curry printf "~a=~a~n"))],
resulting in displays like:

@racketblock[
 #,(racketoutput "X=moria")
 #,(racketoutput "Y=larn")
]

Third, @racket[next] searches for another answer to the last query. If
there was no last query, this evaluates to an error. If there are no
more answers, @litchar{done} is printed.

Here is a sample module:
@racketmod[parenlog
 (type Gamma numConst num)
 (type Gamma boolConst bool)

 (:- (type Gamma (if Test Then Else) Tau)
     (type Gamma Test bool)
     (type Gamma Then Tau)
     (type Gamma Else Tau))

 (? (type mt numConst num)) ; => yes
 (? (type mt (if boolConst numConst numConst) num)) ; => yes
 (? (type mt (if boolConst numConst boolConst) num)) ; => no

 (? (type mt boolConst T)) ; => T=bool
 (? (type mt (if boolConst numConst numConst) T)) ; => T=num
 (? (type mt (if boolConst numConst boolConst) T)) ; => no
]

If this module is evaluated, it prints:
@racketblock[
 #,(racketoutput "yes")
 #,(racketoutput "yes")
 #,(racketoutput "no")
 #,(racketoutput "T=bool")
 #,(racketoutput "T=num")
 #,(racketoutput "no")
]

We can then query the model from the REPL:

@racketinput[next]
@racketblock[#,(racketoutput (tt "done"))]
@racketinput[(type mt T num)]
@racketblock[#,(racketoutput (tt "T=numConst"))]
@racketinput[next]
@racketblock[#,(racketoutput (tt "T=(if boolConst numConst numConst)"))]
@racketinput[next]
@racketblock[#,(racketoutput (tt "T=(if boolConst numConst (if boolConst numConst numConst))"))]

@defidform[?]{
 Syntax that may only appear within the body of a Parenlog module.
}

@defidform[next]{
 Syntax that may only appear within the body of a Parenlog module.
}
