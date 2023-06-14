#lang racket/base


(module+ test
  (require rackunit
           yaragg/base/grammar
           yaragg/base/production-expression
           yaragg/base/semantic-action
           yaragg/base/token
           yaragg/parser
           yaragg/parser/earley))


;@----------------------------------------------------------------------------------------------------


(module+ test
  (test-case "repetition-expression")
  (define start (nonterminal-symbol 'start))
  (define number (atom-symbol 'number))
  (define test-grammar
    (grammar
     #:start-symbol start
     #:rules
     (list
      (production-rule
       #:nonterminal start
       #:action (build-list-action)
       #:substitution (repetition-expression number)))))
  (define parser (earley-parser test-grammar))
  (define input
    (list (atom 'number 1)
          (atom 'number 2)
          (atom 'number 3)
          (atom 'number 4)
          (atom 'number 5)))
  (check-equal? (parse-datum parser input) '(1 2 3 4 5)))
