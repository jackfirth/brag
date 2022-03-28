#lang racket/base
(require yaragg/examples/quotation-marks-and-backslashes
         yaragg/support
         rackunit)

(check-equal? (parse-tree "a\"'\\a\"'\\") '(start "a" "\"" "'" "\\" "a" "\"" "'" "\\"))
