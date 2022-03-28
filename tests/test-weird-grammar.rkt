#lang racket/base

(require yaragg/tests/weird-grammar
         rackunit)

(check-equal? (syntax->datum (parse '("foo")))
              '(foo "foo"))
