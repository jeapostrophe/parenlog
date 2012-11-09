#lang racket/base
(require (for-syntax racket/base
                     racket/list)
         "parenlog.rkt"
         "core.rkt")

(provide (except-out (all-from-out racket/base)
                     #%module-begin
                     #%top-interaction)
         ? next 
         :-
         (rename-out
          [pl-top-interaction #%top-interaction]
          [pl-module-begin #%module-begin]))

(define current-model (box #f))
(define current-generator (box #f))

(define-syntax (? stx)
  (raise-syntax-error '? "Cannot be used outside parenlog language" stx))
(define-syntax (next stx)
  (raise-syntax-error 'next "Cannot be used outside parenlog language" stx))

(define-syntax (pl-module-begin stx)
  (syntax-case stx ()
    [(_ e ...)
     (let-values ([(stmts queries)
                   (partition (lambda (stx)
                                (syntax-case stx (?)
                                  [(? e) #f]
                                  [_ #t]))
                              (syntax->list #'(e ...)))])
       (with-syntax ([(e-stmt ...) stmts]
                     [(e-query ...) 
                      (map (lambda (stx)
                             (syntax-case stx (?)
                               [(? e) #'e]))
                           queries)])
         (quasisyntax/loc stx
           (#%module-begin 
            (define-model this-model
              e-stmt ...)
            (init-generator/find-one this-model e-query)
            ...
            (set-box! current-model this-model)))))]))

(define-syntax (pl-top-interaction stx)
  (syntax-case stx (next)
    [(_ . next)
     (syntax/loc stx
       (get-next-ans))]
    [(_ . form)
     (quasisyntax/loc stx
       (init-generator/find-one (unbox current-model) form))]))

(define-syntax (init-generator/find-one stx)
  (syntax-case stx ()
    [(_ model-expr query-stx)
     (syntax/loc stx
       (begin (set-box! current-generator
                        (query-answer-generator model-expr (compile-query query-stx)))
              (get-next-ans)))]))

(define (get-next-ans)
  (define gen (unbox current-generator))
  (unless gen
    (error 'parenlog "No active query"))
  (call-with-values gen
                    (case-lambda
                      [() (printf "done~n")]
                      [(ans) (print-ans ans)])))

(define (print-ans ans)
  (cond
    [(eq? generator-done ans)
     (printf "no~n")]
    [else
     (if (zero? (hash-count ans))
         (printf "yes~n")
         (for ([(k v) (in-hash ans)])
           (printf "~a=~a~n" k v)))]))
