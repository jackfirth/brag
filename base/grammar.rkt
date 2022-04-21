#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [grammar (-> #:rules (sequence/c production-rule?) #:start-symbol nonterminal-symbol? grammar?)]
  [grammar? predicate/c]
  [production-rule
   (-> #:nonterminal nonterminal-symbol?
       #:action semantic-action?
       #:substitution (or/c production-expression? grammar-symbol?)
       production-rule?)]
  [production-rule? predicate/c]
  [production-expression? predicate/c]
  [group-expression (-> (sequence/c (or/c production-expression? grammar-symbol?)) group-expression?)]
  [group-expression? predicate/c]
  [choice-expression
   (-> (sequence/c (or/c production-expression? grammar-symbol?)) choice-expression?)]
  [choice-expression? predicate/c]
  [repetition-expression
   (->* ((or/c production-expression? grammar-symbol?))
        (#:min-count exact-nonnegative-integer? #:max-count (or/c exact-nonnegative-integer? +inf.0))
        repetition-expression?)]
  [repetition-expression? predicate/c]))


(module+ private
  (provide
   (contract-out
    [grammar-flatten (-> grammar? flat-grammar?)]
    [flat-grammar
     (-> #:rules (sequence/c flat-production-rule?) #:start-symbol nonterminal-symbol? flat-grammar?)]
    [flat-grammar? predicate/c]
    [flat-grammar-rules (-> flat-grammar? (vectorof flat-production-rule? #:immutable #true))]
    [flat-grammar-start-rules (-> flat-grammar? (set/c flat-production-rule? #:kind 'immutable))]
    [flat-grammar-start-symbol (-> flat-grammar? any/c)]
    [flat-production-rule
     (-> #:nonterminal nonterminal-symbol?
         #:substitution (sequence/c grammar-symbol?)
         #:action semantic-action?
         flat-production-rule?)]
    [flat-production-rule? predicate/c]
    [flat-production-rule-action (-> flat-production-rule? semantic-action?)]
    [flat-production-rule-nonterminal (-> flat-production-rule? any/c)]
    [flat-production-rule-substitution
     (-> flat-production-rule? (vectorof grammar-symbol? #:immutable #true))])))


(require racket/match
         racket/sequence
         racket/set
         rebellion/collection/vector
         rebellion/collection/vector/builder
         yaragg/base/semantic-action
         yaragg/base/symbol)


(module+ test
  (require (submod "..")
           rackunit
           rebellion/private/static-name))


;@----------------------------------------------------------------------------------------------------


;; Parsing takes a (Grammar T S A) and a sequence of (Token T V) and produces a set of
;; (Parser-Derivation V A) (also called a "parse forest"). A grammar contains an immutable
;; vector of (Context-Free-Production-Rule T S A) and a start symbol of type S.
;;   T: the terminals the grammar parses. Corresponds to the type field of the input tokens.
;;   S: the nonterminals the grammar rules are defined in terms of.
;;   A: the labels that grammar rules may have attached to them via the (Label-Action A) semantic
;;      action. These show up in parse tree branches, and can be used to determine which production
;;      rule produced a derivation.
(struct flat-grammar (rules start-symbol)
  #:transparent
  #:omit-define-syntaxes
  #:constructor-name constructor:flat-grammar)


(define (flat-grammar-start-rules grammar)
  (define start (flat-grammar-start-symbol grammar))
  (for/set ([rule (in-vector (flat-grammar-rules grammar))]
            #:when (equal? (flat-production-rule-nonterminal rule) start))
    rule))


;; A (Context-Free-Production-Rule T S A) contains a nonterminal symbol of type S, semantic action of
;; type (Semnatic-Action A), and a substitution sequence of (Grammar-Symbol T S) values, stored in an
;; immutable vector.
(struct flat-production-rule (nonterminal action substitution)
  #:transparent
  #:omit-define-syntaxes
  #:constructor-name constructor:flat-production-rule)


(define (flat-grammar #:rules rules #:start-symbol start)
  (constructor:flat-grammar (sequence->vector rules) start))


(define (flat-production-rule #:nonterminal nonterminal
                              #:action action
                              #:substitution substitution)
  (constructor:flat-production-rule nonterminal action (sequence->vector substitution)))


(struct grammar (rules start-symbol)
  #:transparent
  #:guard (λ (rules start-symbol _) (values (sequence->vector rules) start-symbol))
  #:omit-define-syntaxes
  #:constructor-name constructor:grammar)


(define (grammar #:rules rules #:start-symbol start-symbol)
  (constructor:grammar rules start-symbol))


(struct production-rule (nonterminal action substitution)
  #:transparent
  #:omit-define-syntaxes
  #:constructor-name constructor:production-rule)


(define (production-rule #:nonterminal nonterminal #:action action #:substitution substitution)
  (constructor:production-rule nonterminal action substitution))


(struct production-expression () #:transparent)


(struct group-expression production-expression (subexpressions)
  #:transparent
  #:guard (λ (subexpressions _) (sequence->vector subexpressions)))


(struct repetition-expression production-expression (subexpression min-count max-count)
  #:transparent
  #:omit-define-syntaxes
  #:constructor-name constructor:repetition-expression)


(define (repetition-expression subexpression #:min-count [min-count 0] #:max-count [max-count +inf.0])
  (constructor:repetition-expression subexpression min-count max-count))


(struct choice-expression production-expression (choices)
  #:guard (λ (choices _) (sequence->vector choices))
  #:transparent)


(struct virtual-symbol (base-symbol counter) #:transparent)


(define (production-rule-flatten rule)

  (define original-nonterminal (production-rule-nonterminal rule))

  (define new-rules (make-vector-builder))
  (define counter 0)

  (define (fresh-symbol!)
    (define sym
      (nonterminal-symbol (virtual-symbol (nonterminal-symbol-value original-nonterminal) counter)))
    (set! counter (add1 counter))
    sym)

  (define (process-top-level-expression expression)
    (match expression
      [(group-expression subexpressions)
       (for/vector ([expression (in-vector subexpressions)])
         (process-expression expression))]
      [_ (list (process-expression expression))]))

  (define (process-expression expression)
    (match expression
      [(? terminal-symbol?) expression]
      [(? nonterminal-symbol?) expression]

      [(group-expression subexpressions)
       (define subrule-symbol (fresh-symbol!))
       (define group-symbols
         (for/vector ([subexpr (in-vector subexpressions)])
           (process-expression subexpr)))
       (define group-rule
         (flat-production-rule
          #:nonterminal subrule-symbol #:action splice-action #:substitution group-symbols))
       (vector-builder-add new-rules group-rule)
       subrule-symbol]

      [(choice-expression choices)
       (define subrule-symbol (fresh-symbol!))
       (define choice-symbol-vectors
         (for/list ([choice (in-vector choices)])
           (process-top-level-expression choice)))

       ;; We reverse the order the choice symbols are added to ensure the final vector of added rules
       ;; is in the right order.
       (for ([choice-symbols (in-list (reverse choice-symbol-vectors))])
         (define choice-rule
           (flat-production-rule
            #:nonterminal subrule-symbol #:action splice-action #:substitution choice-symbols))
         (vector-builder-add new-rules choice-rule))
       subrule-symbol]

      [(? repetition-expression?)
       #:when (and (equal? (repetition-expression-min-count expression) 0)
                   (equal? (repetition-expression-max-count expression) +inf.0))
       (define subexpression (repetition-expression-subexpression expression))
       (define subrule-symbol (fresh-symbol!))
       (define repetition-symbols (process-top-level-expression subexpression))
       (define empty-rule
         (flat-production-rule
          #:nonterminal subrule-symbol #:action splice-action #:substitution '()))
       (define repetition-rule
         (flat-production-rule
          #:nonterminal subrule-symbol
          #:action splice-action
          #:substitution
          (sequence-append repetition-symbols (list subrule-symbol))))
       (vector-builder-add new-rules empty-rule repetition-rule)
       subrule-symbol]))

  (define processed (process-top-level-expression (production-rule-substitution rule)))

  (define top-level-rule
    (flat-production-rule
     #:nonterminal original-nonterminal
     #:action (production-rule-action rule)
     #:substitution processed))
  (vector-builder-add new-rules top-level-rule)
  (vector-reverse (build-vector new-rules)))


(define (vector-reverse vec)
  (define size (vector-length vec))
  (define copy (make-vector size))
  (for/vector ([i (in-range 0 size)])
    (define j (- size i 1))
    (vector-set! copy j (vector-ref vec i)))
  (vector->immutable-vector copy))


(define (grammar-flatten grammar)
  (define builder (make-vector-builder))
  (for ([rule (in-vector (grammar-rules grammar))])
    (vector-builder-add-all builder (production-rule-flatten rule)))
  (flat-grammar #:rules (build-vector builder) #:start-symbol (grammar-start-symbol grammar)))


(module+ test
  (test-case (name-string production-rule-flatten)

    (define x (nonterminal-symbol 'x))
    (define x.0 (nonterminal-symbol (virtual-symbol 'x 0)))
    (define a (terminal-symbol 'a))
    (define b (terminal-symbol 'b))
    (define c (terminal-symbol 'c))
    (define d (terminal-symbol 'd))

    ;; x: a, (b, c), d
    ;;
    ;; ->
    ;;
    ;; x: a, x.0, d
    ;; x.0: b, c (* splice *)
    (test-case "grouping"
      (define rule
        (production-rule
         #:nonterminal x
         #:action (label-action 'x)
         #:substitution (group-expression (list a (group-expression (list b c)) d))))
      (define expected-rules
        (vector
         (flat-production-rule
          #:nonterminal x #:action (label-action 'x) #:substitution (list a x.0 d))
         (flat-production-rule #:nonterminal x.0 #:action splice-action #:substitution (list b c))))
      (check-equal? (production-rule-flatten rule) expected-rules))

    ;; x: a, (b | c), d
    ;;
    ;; ->
    ;;
    ;; x: a, x.0, d
    ;; x.0: b (* splice *)
    ;; x.0: c (* splice *)
    (test-case "choice"
      (define rule
        (production-rule
         #:nonterminal x
         #:action (label-action 'x)
         #:substitution (group-expression (list a (choice-expression (list b c)) d))))
      (define expected-rules
        (vector
         (flat-production-rule
          #:nonterminal x #:action (label-action 'x) #:substitution (list a x.0 d))
         (flat-production-rule #:nonterminal x.0 #:action splice-action #:substitution (list b))
         (flat-production-rule #:nonterminal x.0 #:action splice-action #:substitution (list c))))
      (check-equal? (production-rule-flatten rule) expected-rules))

    ;; x: a b*
    ;;
    ;; ->
    ;;
    ;; x: a x.0
    ;; x.0: b x.0 (* splice *)
    ;; x.0: (* splice *)
    (test-case "repeating"
      (define rule
        (production-rule
         #:nonterminal x
         #:action (label-action 'x)
         #:substitution (group-expression (list a (repetition-expression b) c))))
      (define expected-rules
        (vector
         (flat-production-rule
          #:nonterminal x #:action (label-action 'x) #:substitution (list a x.0 c))
         (flat-production-rule
          #:nonterminal x.0
          #:action splice-action
          #:substitution (list b x.0))
         (flat-production-rule
          #:nonterminal x.0
          #:action splice-action
          #:substitution '())))
      (check-equal? (production-rule-flatten rule) expected-rules))))
