#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [grammar-symbol? predicate/c]
  [atom-symbol (-> symbol? atom-symbol?)]
  [atom-symbol? (-> any/c boolean?)]
  [atom-symbol-type (-> atom-symbol? symbol?)]
  [punctuation-symbol (-> string? punctuation-symbol?)]
  [punctuation-symbol? (-> any/c boolean?)]
  [punctuation-symbol-string (-> punctuation-symbol? (and/c string? immutable?))]
  [nonterminal-symbol (-> any/c nonterminal-symbol?)]
  [nonterminal-symbol? predicate/c]
  [nonterminal-symbol-value (-> nonterminal-symbol? any/c)]
  [production-expression? predicate/c]
  [group-expression (-> (sequence/c production-expression?) group-expression?)]
  [group-expression? predicate/c]
  [group-expression-subexpressions
   (-> group-expression? (vectorof production-expression? #:immutable #true))]
  [choice-expression (-> (sequence/c production-expression?) choice-expression?)]
  [choice-expression? predicate/c]
  [choice-expression-choices
   (-> choice-expression? (vectorof production-expression? #:immutable #true))]
  [repetition-expression
   (->* (production-expression?)
        (#:min-count exact-nonnegative-integer? #:max-count (or/c exact-nonnegative-integer? +inf.0))
        repetition-expression?)]
  [repetition-expression? predicate/c]
  [repetition-expression-subexpression (-> repetition-expression? production-expression?)]
  [repetition-expression-min-count (-> repetition-expression? exact-nonnegative-integer?)]
  [repetition-expression-max-count
   (-> repetition-expression? (or/c exact-nonnegative-integer? +inf.0))]
  [cut-expression (-> production-expression? cut-expression?)]
  [cut-expression? predicate/c]
  [cut-expression-subexpression (-> cut-expression? production-expression?)]))


(require racket/sequence
         rebellion/collection/vector)


;@----------------------------------------------------------------------------------------------------


(struct production-expression () #:transparent)


(struct group-expression production-expression (subexpressions)
  #:transparent
  #:guard (λ (subexpressions _) (sequence->vector subexpressions)))


(struct choice-expression production-expression (choices)
  #:guard (λ (choices _) (sequence->vector choices))
  #:transparent)


(struct repetition-expression production-expression (subexpression min-count max-count)
  #:transparent
  #:omit-define-syntaxes
  #:constructor-name constructor:repetition-expression)


(define (repetition-expression subexpression #:min-count [min-count 0] #:max-count [max-count +inf.0])
  (constructor:repetition-expression subexpression min-count max-count))


(struct cut-expression production-expression (subexpression) #:transparent)

(struct grammar-symbol production-expression () #:transparent)
(struct atom-symbol grammar-symbol (type) #:transparent)
(struct punctuation-symbol grammar-symbol (string) #:transparent)
(struct nonterminal-symbol grammar-symbol (value) #:transparent)
