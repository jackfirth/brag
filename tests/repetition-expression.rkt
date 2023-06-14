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
  (define start (nonterminal-symbol 'start))
  (define s1 (nonterminal-symbol 's1))
  (define number (atom-symbol 'number))

  (test-case "manual repetition with right recursion"
    (define g
      (grammar
       #:start-symbol start
       #:rules
       (list
        (production-rule #:nonterminal start #:action (build-list-action) #:substitution s1)
        (production-rule
         #:nonterminal s1 #:action splice-action #:substitution (group-expression '()))
        (production-rule
         #:nonterminal s1
         #:action splice-action
         #:substitution (group-expression (list number s1))))))
    (define parser (earley-parser g))
    (define input
      (list (atom 'number 1)
            (atom 'number 2)
            (atom 'number 3)
            (atom 'number 4)
            (atom 'number 5)))
    (check-equal? (parse-datum parser input) '(1 2 3 4 5)))

  (test-case "manual repetition with left recursion"
    (define g
      (grammar
       #:start-symbol start
       #:rules
       (list
        (production-rule #:nonterminal start #:action (build-list-action) #:substitution s1)
        (production-rule
         #:nonterminal s1 #:action splice-action #:substitution (group-expression '()))
        (production-rule
         #:nonterminal s1
         #:action splice-action
         #:substitution (group-expression (list s1 number))))))
    (define parser (earley-parser g))
    (define input
      (list (atom 'number 1)
            (atom 'number 2)
            (atom 'number 3)
            (atom 'number 4)
            (atom 'number 5)))
    (check-equal? (parse-datum parser input) '(1 2 3 4 5)))

  (test-case "repetition-expression"
    (define g
      (grammar
       #:start-symbol start
       #:rules
       (list
        (production-rule
         #:nonterminal start
         #:action (build-list-action)
         #:substitution (repetition-expression number)))))
    (define parser (earley-parser g))
    (define input
      (list (atom 'number 1)
            (atom 'number 2)
            (atom 'number 3)
            (atom 'number 4)
            (atom 'number 5)))
    (check-equal? (parse-datum parser input) '(1 2 3 4 5))))
  