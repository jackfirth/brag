#lang racket/base
(require (prefix-in 1: yaragg/examples/top-level-cut-1)
         (prefix-in 2: yaragg/examples/top-level-cut-2)
         (prefix-in 3: yaragg/examples/top-level-cut-3)
         yaragg/support
         rackunit)

(check-equal? (1:parse-to-datum "x") '((sub "x")))
(check-equal? (2:parse-to-datum "x") '(("x")))
(check-equal? (3:parse-to-datum "x") '("x"))

