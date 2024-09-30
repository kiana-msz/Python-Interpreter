#lang racket

(require "datatypes.rkt")
(require (lib "eopl.ss" "eopl"))

(define empty_env (lambda () (empty_environment)))

(define update_env (lambda (var val env) (cases environment env
    (empty_environment () (extend_environment var val env))
    (extend_environment (saved_var saved_val saved_env)
        (if (equal? var saved_var)
            (extend_environment var val saved_env)
            (extend_environment saved_var saved_val (update_env var val saved_env)))))))

(define apply_env (lambda (var env) (cases environment env
    (empty_environment () (eopl:error 'binding-error!
        "\n\tidentifier ~s is used before declaration" var))
    (extend_environment (saved_var val saved_env)
        (if (equal? var saved_var) val (apply_env var saved_env))))))

 (define exists-var? (lambda(var env) (cases environment env
     (empty_environment () #f)
     (extend_environment (saved_var val saved_env)
         (if (equal? var saved_var)
             #t
             (exists-var? var saved_env)
         ))
 )))

(provide (all-defined-out))