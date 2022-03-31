#lang racket/base


(require racket/contract
         racket/match
         racket/sequence
         racket/set
         racket/struct
         rebellion/collection/vector
         rebellion/private/guarded-block)


(module+ test
  (require (submod "..")
           rackunit))


;; Parsing takes a (Grammar T S L) and a sequence of (Token T V) and produces a set of
;; (Parser-Derivation V S L) (also called a "parse forest"). A grammar contains an immutable
;; vector of (Context-Free-Production-Rule T S L) and a start symbol of type S.
;;   T: the terminals the grammar parses. Corresponds to the type field of the input tokens.
;;   S: the nonterminals the grammar rules are defined in terms of. These show up in parse tree
;;      branches.
;;   L: the lables that grammar rules may have attached to them. These show up in parse tree
;;      branches alongside nonterminals, and can be used to determine which production rule for a
;;      particular nonterminal produced a derivation.
(struct context-free-grammar (rules start-symbol) #:transparent)


(define (grammar-start-rules grammar)
  (define start (context-free-grammar-start-symbol grammar))
  (for/set ([rule (in-vector (context-free-grammar-rules grammar))]
            #:when (equal? (context-free-production-rule-nonterminal rule) start))
    rule))


;; A (Token T V) is a tagged value. The grammar rules are defined in terms of the type tag,
;; whereas the value is what appears in leaf nodes of the resulting parse trees.
(struct token (type value) #:transparent)


;; A (Context-Free-Production-Rule T S L) contains a nonterminal symbol of type S, a label of type L,
;; and a substitution sequence of (Grammar-Symbol T S) values, stored in an immutable vector.
(struct context-free-production-rule (nonterminal label substitution) #:transparent)


;; A (Grammar-Symbol T S) is either a (Terminal-Symbol T) or a (Nonterminal-Symbol S)
(struct grammar-symbol () #:transparent)
(struct terminal-symbol grammar-symbol (value) #:transparent)
(struct nonterminal-symbol grammar-symbol (value) #:transparent)


(define (make-grammar #:rules rules #:start-symbol start)
  (context-free-grammar (sequence->vector rules) start))


(define (make-rule #:symbol symbol #:substitution substitution #:label label)
  (context-free-production-rule symbol label (sequence->vector substitution)))


;; A (Parser-Derivation V S L) is either a (Terminal-Derivation V) or a (Nonterminal-Derivation T S L)
(struct parser-derivation () #:transparent)

;; A (Terminal-Derivation V) represents a terminal that was matched by the grammar. It contains the
;; value of the (Token T V) that was matched.
(struct terminal-derivation parser-derivation (value) #:transparent)

;; A (Nonterminal-Derivation T S L) represents a nonterminal that was matched by the grammar. It
;; contains the nonterminal symbol of type T that was matched, the label of type L of the specific
;; rule that matched, and an immutable vector of subderivations 
(struct nonterminal-derivation parser-derivation (symbol label children) #:transparent)


(define (make-nonterminal-derivation symbol label [children '()])
  (nonterminal-derivation symbol label (sequence->vector children)))


(define derivation
  (case-lambda
    [(value) (terminal-derivation value)]
    [(symbol label . children) (make-nonterminal-derivation symbol label children)]))


(define a (terminal-symbol 'a))
(define b (terminal-symbol 'b))
(define S (nonterminal-symbol 'S))


(define as-then-bs-grammar
  (make-grammar
   #:rules
   (list
    (make-rule #:symbol (nonterminal-symbol-value S) #:substitution (list a S b) #:label 'recur)
    (make-rule #:symbol (nonterminal-symbol-value S) #:substitution (list a b) #:label 'done))
   #:start-symbol (nonterminal-symbol-value S)))


(define input
  (list (token 'a 'a1) (token 'a 'a2) (token 'a 'a3) (token 'b 'b1) (token 'b 'b2) (token 'b 'b3)))


(define expected-parse-tree
  (derivation 'S 'recur
              (derivation 'a1)
              (derivation 'S 'recur
                          (derivation 'a2)
                          (derivation 'S 'done (derivation 'a3) (derivation 'b1))
                          (derivation 'b2))
              (derivation 'b3)))


;; Earley parser


(struct earley-state (rule substitution-position input-position)
  #:transparent
  #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (_) 'earley-state)
      (λ (this)
        (define rule (earley-state-rule this))
        (define substitution (context-free-production-rule-substitution rule))
        (define pos (earley-state-substitution-position this))
        (list (list (context-free-production-rule-nonterminal rule) '->)
              (for/list ([sym (in-vector substitution 0 pos)])
                (if (terminal-symbol? sym)
                    (terminal-symbol-value sym)
                    (nonterminal-symbol-value sym)))
              (list '•)
              (for/list ([sym (in-vector substitution pos)])
                (if (terminal-symbol? sym)
                    (terminal-symbol-value sym)
                    (nonterminal-symbol-value sym)))
              (earley-state-input-position this)))))])


(define (initial-earley-states grammar)
  (for/set ([rule (grammar-start-rules grammar)])
    (earley-state rule 0 0)))


(define (earley-parse grammar token-sequence)
  (define tokens (sequence->vector token-sequence))
  (define token-count (vector-length tokens))
  (define position-count (add1 token-count))
  (define states (make-vector position-count (set)))
  (vector-set! states 0 (initial-earley-states grammar))
  (for ([k (in-range 0 position-count)])
    
    ;; Prediction and completion
    (define/guard (process-states unprocessed processed)
      (guard (set-empty? unprocessed) then
        processed)
      (define next (set-first unprocessed))
      
      (define added-states
        (guarded-block
         (guard (completed-state? next) then
           ;; find all states in S(j) of the form (X → α • Y β, j) and add (X → α Y • β, j)
           (define j (earley-state-input-position next))
           (define completed (context-free-production-rule-nonterminal (earley-state-rule next)))
           (define parent-states
             (if (equal? j k)
                 (set-union unprocessed processed)
                 (vector-ref states j)))
           (completer-states completed parent-states))
         (define symbol (earley-state-next-symbol next))
         (guard (nonterminal-symbol? symbol) else
           (set))
         (predictor-states grammar (nonterminal-symbol-value symbol) k)))
      
      (define new-unprocessed (set-subtract (set-remove added-states next) processed))
      (process-states (set-union (set-rest unprocessed) new-unprocessed) (set-add processed next)))

    (define processed (process-states (vector-ref states k) (set)))
    (vector-set! states k processed)
    (unless (equal? k token-count)
      (vector-set! states (add1 k) (scanner-states processed k (vector-ref tokens k)))))
  states)


(define (completed-state? state)
  (match-define (earley-state rule substitution-position _) state)
  (equal? substitution-position
          (vector-length (context-free-production-rule-substitution rule))))


(define/contract (earley-state-next-symbol state)
  (-> (and/c earley-state? (not/c completed-state?)) grammar-symbol?)
  (match-define (earley-state rule substitution-position _) state)
  (vector-ref (context-free-production-rule-substitution rule) substitution-position))


(define (earley-state-advance-substitution state)
  (match-define (earley-state rule substitution-position input-position) state)
  (earley-state rule (add1 substitution-position) input-position))


(define (completer-states completed-nonterminal states)
  (for/set ([s (in-set states)]
            #:when (equal? (earley-state-next-symbol s) (nonterminal-symbol completed-nonterminal)))
    (earley-state-advance-substitution s)))


(define (predictor-states grammar nonterminal k)
  ;; add (Y → • γ, k) for every production in the grammar with Y on the left-hand side
  (for/set ([rule (in-vector (context-free-grammar-rules grammar))]
            #:when (equal? (context-free-production-rule-nonterminal rule) nonterminal))
    (earley-state rule 0 k)))


(define (scanner-states states k next-token)
  (define type (token-type next-token))
  (define expected (terminal-symbol type))
  (for/set ([s (in-set states)]
            #:when (not (completed-state? s))
            #:when (equal? (earley-state-next-symbol s) expected))
    (earley-state-advance-substitution s)))



(module+ test
  (test-case "earley-parse integration test"

    ;; Grammar, input, and states taken from https://en.wikipedia.org/wiki/Earley_parser#Example
    (define P-rule (make-rule #:symbol 'P #:label 0 #:substitution (list (nonterminal-symbol 'S))))
    (define S-rule0
      (make-rule
       #:symbol 'S
       #:label 0
       #:substitution (list (nonterminal-symbol 'S) (terminal-symbol '+) (nonterminal-symbol 'M))))
    (define S-rule1 (make-rule #:symbol 'S #:label 1 #:substitution (list (nonterminal-symbol 'M))))
    (define M-rule0
      (make-rule
       #:symbol 'M
       #:label 0
       #:substitution (list (nonterminal-symbol 'M) (terminal-symbol '*) (nonterminal-symbol 'T))))
    (define M-rule1 (make-rule #:symbol 'M #:label 1 #:substitution (list (nonterminal-symbol 'T))))
    (define T-rule (make-rule #:symbol 'T #:label 0 #:substitution (list (terminal-symbol 'number))))
    (define arithmetic-grammar
      (make-grammar #:rules (list P-rule S-rule0 S-rule1 M-rule0 M-rule1 T-rule) #:start-symbol 'P))
    (define input-tokens
      (list (token 'number 2) (token '+ '+) (token 'number 3) (token '* '*) (token 'number 4)))

    (define arithmetic-parse-table
      (earley-parse arithmetic-grammar input-tokens))

    (define expected-arithmetic-parse-table
      (vector
       (set
        (earley-state P-rule 0 0)
        (earley-state S-rule0 0 0)
        (earley-state S-rule1 0 0)
        (earley-state M-rule0 0 0)
        (earley-state M-rule1 0 0)
        (earley-state T-rule 0 0))
       (set
        (earley-state T-rule 1 0)
        (earley-state M-rule0 1 0)
        (earley-state M-rule1 1 0)
        (earley-state S-rule0 1 0)
        (earley-state S-rule1 1 0)
        (earley-state P-rule 1 0))
       (set
        (earley-state S-rule0 2 0)
        (earley-state M-rule0 0 2)
        (earley-state M-rule1 0 2)
        (earley-state T-rule 0 2))
       (set
        (earley-state T-rule 1 2)
        (earley-state M-rule0 1 2)
        (earley-state M-rule1 1 2)
        (earley-state S-rule0 3 0)
        (earley-state S-rule0 1 0)
        (earley-state P-rule 1 0))
       (set
        (earley-state M-rule0 2 2)
        (earley-state T-rule 0 4))
       (set
        (earley-state T-rule 1 4)
        (earley-state M-rule0 3 2)
        (earley-state M-rule0 1 2)
        (earley-state S-rule0 3 0)
        (earley-state S-rule0 1 0)
        (earley-state P-rule 1 0))))
    (check-equal? arithmetic-parse-table expected-arithmetic-parse-table)))
