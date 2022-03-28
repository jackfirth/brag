#lang racket/base

(module+ test

  (require yaragg/examples/cutter
           yaragg/support
           rackunit)

  ;; related to rule-flattening problem
  (check-equal?
   (parse-to-datum (list "(" "x" "," "x" ")"))
   '(top (expr (list "(" (expr "x") "," (expr "x") ")")))))
