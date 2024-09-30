#lang racket
(require (lib "eopl.ss" "eopl"))
(require "datatypes.rkt")
(require "environment.rkt")


(define scopes '())

(define scope-index? (lambda (n) (< n (length scopes))))

(define-datatype scope scope?
  (the-scope (env environment?) (upper-scope-index scope-index?) (globals list?) (params list?))
  )

(define (scope->env s)
  (cases scope s
    (the-scope (env upper-scope-index globals params) env )
    ))

(define (scope->upper s)
  (cases scope s
    (the-scope (env upper-scope-index globals params) upper-scope-index )
    ))

(define (scope->globals s)
  (cases scope s
    (the-scope (env upper-scope-index globals params) globals )
))

(define (scope->params s)
  (cases scope s
    (the-scope (env upper-scope-index globals params) params)
    ))

(define (extend-scope-globals scope-index var)
    (let ([-scope (get-scope scope-index)])
        (set-scope scope-index
            (the-scope (scope->env -scope) (scope->upper -scope) (append (scope->globals -scope) (list var)) (scope->params -scope))
        )
    )
  )


(define correct-index? (lambda (n) (< n (length scopes))))

(define (get-scope index)
  (get-scope-on-given-scopes index scopes)
)

(define (get-scope-on-given-scopes index -scopes)
  (list-ref -scopes index)
)

(define (set-scope index s)
  (set! scopes
        (list-set scopes index s)
        ))

(define (add-scope s)
  (begin
    (set! scopes (append scopes (list s)))
    (- (length scopes) 1)
    ))

(define (renew-scope) (set! scopes '()))

(define (set-scopes -scopes) (set! scopes -scopes))

(define (init-scope) (the-scope (empty_environment) -1 '() '() ))

(define (apply-scope scope-index var)
  (apply-scope-on-given-scopes scope-index scopes var)
)

(define (apply-scope-on-given-scopes scope-index -scopes var)
    (
   let ([my-scope (get-scope-on-given-scopes scope-index -scopes)])
    (
     if (is-global-on-given-scopes? var scope-index -scopes) 
     (apply-scope-on-given-scopes 0 -scopes var)
     (if (is-param-on-given-scopes? var scope-index -scopes)
        (get-param-on-given-scopes var scope-index -scopes)
     (
      let ([res (apply_env var (scope->env my-scope))])
       (cond
         [(not (equal? res (empty_environment))) res]
         [(>= (scope->upper my-scope) 0) (apply-scope-on-given-scopes (scope->upper my-scope) -scopes var)]
         [else (eopl:error 'binding-error!
            "\n\tIdentifier ~s is used before declaration!" var)]
         )
      ))
     )
    )
)

(define (extend-scope-param scope-index var value)
    (let ([current-scope (get-scope scope-index)])
        (let ([current-env (scope->env current-scope)])
            (let ([new-env (update_env var value current-env)])
            (begin
                (set-scope scope-index
                    (the-scope new-env
                        (scope->upper current-scope)
                        (scope->globals current-scope)
                        (append (scope->params current-scope) (list var))
                    )
                )
                )
            )
        )
    )
)


(define (extend-scope scope-index var value is-param)
    (if is-param
        (extend-scope-param scope-index var value)
    (let ([current-scope (get-scope scope-index)])
        (let ([current-env (scope->env current-scope)])
            (let ([new-env (update_env var value current-env)])
            (begin
                (set-scope scope-index 
                    (the-scope new-env 
                        (scope->upper current-scope)
                        (scope->globals current-scope)
                        (scope->params current-scope)
                    )
                )
                (if (and (not (is-param-in-scope scope-index var)) (>= (scope->upper current-scope) 0) (is-defined? var (scope->upper current-scope)))
                    (extend-scope (scope->upper current-scope) var value is-param)
                    null
                )
                )
            )
        )
    ))
)

(define (child-scope parent-index)
    (let ([parent (get-scope parent-index)])
     (the-scope (scope->env parent) parent-index (scope->globals parent) '())
     )
)

(define (is-defined? var scope-index)
    (let ([my-scope (get-scope scope-index)])
         (exists-var? var (scope->env my-scope))
    )
)

(define (is-global? var scope-index)
  (is-global-on-given-scopes? var scope-index scopes)
)

(define (is-global-on-given-scopes? var scope-index -scopes)
  (member var (scope->globals (get-scope-on-given-scopes scope-index -scopes)))
)

(define (is-param-on-given-scopes? var scope-index -scopes)
  (member var (scope->params (get-scope-on-given-scopes scope-index -scopes)))
)

(define (get-param-on-given-scopes var scope-index -scopes)
    (let ([my-scope (get-scope-on-given-scopes scope-index -scopes)])
      (apply_env var (scope->env my-scope))
    )
  )


(define (is-param-in-scope scope-index var)
    (member var (scope->params (get-scope scope-index)))
)

(define-datatype proc proc?
  (new-proc
   (params eval-func-param*?)
   (statements list?)
   (parent-scope scope-index?)
   )
  )

(define-datatype eval-func-param eval-func-param?
  (eval_with_default (var string?) (val (lambda (x) #t)))
  )

(define-datatype eval-func-param* eval-func-param*?
  (empty-eval-func-param)
  (eval-func-params (eval-param eval-func-param?) (rest-evals eval-func-param*?))
  )

(provide (all-defined-out))