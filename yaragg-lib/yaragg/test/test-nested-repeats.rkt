#lang racket/base
(require yaragg/examples/nested-repeats
         rackunit)

(check-equal?
 (syntax->datum (parse (list "X" "Y" "X")))
 '(start "X" "Y" "X"))

               
