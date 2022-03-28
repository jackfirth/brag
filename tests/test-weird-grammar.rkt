#lang racket/base

(require yaragg/test/weird-grammar
         rackunit)

(check-equal? (syntax->datum (parse '("foo")))
              '(foo "foo"))
