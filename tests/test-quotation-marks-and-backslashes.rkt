#lang racket/base

(module+ test

  (require yaragg/examples/quotation-marks-and-backslashes
           yaragg/support
           rackunit)

  (check-equal? (parse-tree "a\"'\\a\"'\\") '(start "a" "\"" "'" "\\" "a" "\"" "'" "\\")))
