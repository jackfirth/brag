#lang racket/base

(require yaragg/examples/codepoints
         rackunit)

(check-equal? (parse-to-datum '("\"A\\" "'c\\" "*d\\\"\\ef\"" "hello world"))
              '(start (A "\"A\\")
                      (c "'c\\")
                      (def "*d\\\"\\ef\"")
                      (hello-world "hello world")))
