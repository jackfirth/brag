#lang racket/base

(require brag/test/weird-grammar
         rackunit)

(check-equal? (syntax->datum (parse '("foo")))
              '(foo "foo"))
