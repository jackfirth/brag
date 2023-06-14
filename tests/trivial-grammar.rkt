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
  (define-nonterminals start s1 s2)
  (define-atoms number string)
  
  (test-case "single rule grammar"
    (define g
      (grammar
       #:start-symbol start
       #:rules
       (list
        (production-rule #:nonterminal start #:action (label-action 'start) #:substitution number))))
    (define p (earley-parser g))
    (check-equal? (parse-datum p (list (atom 'number 1))) '(start 1)))

  (test-case "multi-rule grammar"
    (define g
      (grammar
       #:start-symbol start
       #:rules
       (list
        (production-rule #:nonterminal start #:action (label-action 'start) #:substitution s1)
        (production-rule #:nonterminal s1 #:action (label-action 's1) #:substitution s2)
        (production-rule #:nonterminal s2 #:action (label-action 's2) #:substitution number))))
    (define p (earley-parser g))
    (check-equal? (parse-datum p (list (atom 'number 1))) '(start (s1 (s2 1)))))

  (test-case "multi-branch grammar"
    (define g
      (grammar
       #:start-symbol start
       #:rules
       (list
        (production-rule #:nonterminal start #:action (label-action 'start) #:substitution s1)
        (production-rule #:nonterminal s1 #:action (label-action 'branch1) #:substitution number)
        (production-rule #:nonterminal s1 #:action (label-action 'branch2) #:substitution string))))
    (define p (earley-parser g))
    (check-equal? (parse-datum p (list (atom 'number 1))) '(start (branch1 1)))
    (check-equal? (parse-datum p (list (atom 'string "foo"))) '(start (branch2 "foo"))))

  (test-case "empty grammar"
    (define g
      (grammar
       #:start-symbol start
       #:rules
       (list
        (production-rule
         #:nonterminal start #:action (label-action 'start) #:substitution (group-expression '())))))
    (define p (earley-parser g))
    (check-equal? (parse-datum p '()) '(start))))
