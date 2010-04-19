#lang scribble/doc
@(require scribble/manual
          scribble/eval
          scribble/basic
          scribble/bnf
          (planet cce/scheme:6:3/planet)
          (planet cce/scheme:6:3/scribble)
          (for-label scheme/base
                     scheme/function
                     scheme/contract
                     "main.ss"
                     (only-in "lang.ss" ? next)))

@(define the-eval
   (let ([the-eval (make-base-eval)])
     (the-eval `(require (planet ,(this-package-version-symbol))))
     the-eval))

@title[#:tag "top"]{@bold{Parenlog}}
@author[(author+email "Jay McCarthy" "jay@plt-scheme.org")]

This package contains an implementation of a language very similar to pure @link["http://en.wikipedia.org/wiki/Prolog"]{Prolog}, except with parenthetical notation.

@section{Embedded Parenlog}

The easiest way to get started using Parenlog for PLT Scheme is with the main module:

@defmodule/this-package[]

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
                       (,(compose not equal?) X Y)))
                 (query-model family-tree
                              (sibling adom zapm))
                 (query-model family-tree
                              #:limit 4
                              (sibling X Y))]

@defform/subs[#:literals (:- unquote)
              (define-model id stmt ...)
              ([stmt head-query
                     (:- head-query body-query ...)]
               [head-query s-expr]
               [body-query s-expr
                           (,fun s-expr ...)])
              #:contracts
              ([id identifier?]
               [fun (any/c ... -> boolean?)])]{
 Defines @scheme[id] as a Parenlog model.
}
                                              
@defidform[:-]{
 Syntax that may only appear within @scheme[define-model].
}

@defform/subs[(query-model model-expr maybe-limit body-query)
              ([maybe-limit code:blank
                            (code:line #:limit limit-expr)])
              #:contracts
              ([model-expr model?]
               [limit-expr number?])]{
 Queries @scheme[model-expr] with @scheme[body-query] until @scheme[limit-expr] results are found
 or no results remain.
 
 Returns a value matching the contract: @scheme[(listof (hash/c symbol? anc/c))]. Each value in this list is a
 substitution of @scheme[body-query] that @scheme[model-expr] proves.
}
                                     
@defproc[(model? [v any/c]) boolean?]{
 Returns @scheme[#t] if @scheme[v] was bound by @scheme[define-model]; @scheme[#f] otherwise.
}

@section{Standalone Parenlog}

@defmodule/this-package[lang]

Parenlog can also be used as a standalone module language.

@(scheme #,(hash-lang) planet #,(this-package-version-symbol))

At a high level, the body of a Parenlog module is the body of a @scheme[define-model] form and any REPL interaction is placed within a
@scheme[query-model] form. There are, of course, a few caveats.

First, occurrences of @scheme[(? body-query)] in the module body are delayed and used as queries.

Second, all query results are printed. @litchar{no} is printed when there are no answeres. @litchar{yes} is printed when there is an empty
substitution as an answer. Otherwise, the substitution is printed using @scheme[(hash-for-each _subst (curry printf "~a=~a~n"))], resulting in
displays like:
@schemeblock[
 #,(schemeoutput "X=moria")
 #,(schemeoutput "Y=larn")
]

Third, @scheme[next] can be input at the REPL to search for another answer to the last query. If there was no last query, this evaluates to an error. If there are no more answers, @litchar{done} is printed.

Here is a sample module:
@schemeblock[
 #,(hash-lang) planet #,(this-package-version-symbol)
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
@schemeblock[
 #,(schemeoutput "yes")
 #,(schemeoutput "yes")
 #,(schemeoutput "no")
 #,(schemeoutput "T=bool")
 #,(schemeoutput "T=num")
 #,(schemeoutput "no")
]

We can then query the model from the REPL:

@schemeinput[next]
@schemeblock[#,(schemeoutput (tt "done"))]
@schemeinput[(type mt T num)]
@schemeblock[#,(schemeoutput (tt "T=numConst"))]
@schemeinput[next]
@schemeblock[#,(schemeoutput (tt "T=(if boolConst numConst numConst)"))]
@schemeinput[next]
@schemeblock[#,(schemeoutput (tt "T=(if boolConst numConst (if boolConst numConst numConst))"))]

@defidform[?]{
 Syntax that may only appear within the body of a Parenlog module.
}

@defidform[next]{
 Syntax that may only appear within the REPL of a Parenlog module.
}