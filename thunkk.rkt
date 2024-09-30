#lang racket
(require (lib "eopl.ss" "eopl"))
(require "datatypes.rkt")
(require "environment.rkt")
(require "scoping.rkt")


(define-datatype thunkk thunkk?
    (a-thunkk (expr expression?) (scopes-state list?) (scope-index number?))
)

(define (thunkk->scopes-state th)
    (cases thunkk th
        (a-thunkk (expr scopes-state scope-index) scopes-state)
    )
)

(define (thunkk->scope-index th)
    (cases thunkk th
        (a-thunkk (expr scopes-state scope-index) scope-index)
    )
)

(define (thunkk->expr th)
    (cases thunkk th
        (a-thunkk (expr scopes-state scope-index) expr)
    )
)


(provide (all-defined-out))
