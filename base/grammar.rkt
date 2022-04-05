#lang racket/base


(require racket/contract/base)


(provide
 (struct-out terminal-symbol)
 (struct-out nonterminal-symbol)
 (struct-out context-free-grammar)
 (struct-out context-free-production-rule)
 (contract-out
  [grammar-symbol? predicate/c]
  [grammar-start-rules
   (-> context-free-grammar? (set/c context-free-production-rule? #:kind 'immutable))]
  [make-grammar
   (-> #:rules (sequence/c context-free-production-rule?) #:start-symbol any/c context-free-grammar?)]
  [make-rule
   (-> #:symbol any/c #:substitution (sequence/c grammar-symbol?) #:label any/c
       context-free-production-rule?)]))


(require racket/sequence
         racket/set
         rebellion/collection/vector)


;@----------------------------------------------------------------------------------------------------


;; Parsing takes a (Grammar T S L) and a sequence of (Token T V) and produces a set of
;; (Parser-Derivation V L) (also called a "parse forest"). A grammar contains an immutable
;; vector of (Context-Free-Production-Rule T S L) and a start symbol of type S.
;;   T: the terminals the grammar parses. Corresponds to the type field of the input tokens.
;;   S: the nonterminals the grammar rules are defined in terms of.
;;   L: the labels that grammar rules may have attached to them. These show up in parse tree
;;      branches, and can be used to determine which production rule produced a derivation.
(struct context-free-grammar (rules start-symbol) #:transparent)


(define (grammar-start-rules grammar)
  (define start (context-free-grammar-start-symbol grammar))
  (for/set ([rule (in-vector (context-free-grammar-rules grammar))]
            #:when (equal? (context-free-production-rule-nonterminal rule) start))
    rule))


;; A (Context-Free-Production-Rule T S L) contains a nonterminal symbol of type S, a label of type L,
;; and a substitution sequence of (Grammar-Symbol T S) values, stored in an immutable vector.
(struct context-free-production-rule (nonterminal label substitution) #:transparent)


;; A (Grammar-Symbol T S) is either a (Terminal-Symbol T) or a (Nonterminal-Symbol S)
(define (grammar-symbol? v)
  (or (terminal-symbol? v) (nonterminal-symbol? v)))


(struct terminal-symbol (value) #:transparent)
(struct nonterminal-symbol (value) #:transparent)


(define (make-grammar #:rules rules #:start-symbol start)
  (context-free-grammar (sequence->vector rules) start))


(define (make-rule #:symbol symbol #:substitution substitution #:label label)
  (context-free-production-rule symbol label (sequence->vector substitution)))
