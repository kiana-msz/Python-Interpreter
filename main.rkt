#lang racket

(require "passes/parser.rkt")
(require "passes/lexer.rkt")
(require "interpreter.rkt")
(require "environment.rkt")
(require "datatypes.rkt")


(define (parse-scan prog-string)
  (python-parser (lex-this prog-string))
  )

(define (evaluate file-name)
  (interpret-program (parse-scan (string-join (file->lines file-name))) (empty_env))
  )

(provide (all-defined-out))