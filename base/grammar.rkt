#lang racket/base


(require racket/contract/base)


(provide
 (struct-out terminal-symbol)
 (struct-out nonterminal-symbol)
 (struct-out cf-grammar)
 (struct-out cf-production-rule)
 (contract-out
  [grammar-symbol? predicate/c]
  [cf-grammar-start-rules (-> cf-grammar? (set/c cf-production-rule? #:kind 'immutable))]
  [make-cf-grammar (-> #:rules (sequence/c cf-production-rule?) #:start-symbol any/c cf-grammar?)]
  [make-cf-production-rule
   (-> #:symbol any/c #:substitution (sequence/c grammar-symbol?) #:label any/c
       cf-production-rule?)]))


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
(struct cf-grammar (rules start-symbol) #:transparent)


(define (cf-grammar-start-rules grammar)
  (define start (cf-grammar-start-symbol grammar))
  (for/set ([rule (in-vector (cf-grammar-rules grammar))]
            #:when (equal? (cf-production-rule-nonterminal rule) start))
    rule))


;; A (Context-Free-Production-Rule T S L) contains a nonterminal symbol of type S, a label of type L,
;; and a substitution sequence of (Grammar-Symbol T S) values, stored in an immutable vector.
(struct cf-production-rule (nonterminal label substitution) #:transparent)


;; A (Grammar-Symbol T S) is either a (Terminal-Symbol T) or a (Nonterminal-Symbol S)
(define (grammar-symbol? v)
  (or (terminal-symbol? v) (nonterminal-symbol? v)))


(struct terminal-symbol (value) #:transparent)
(struct nonterminal-symbol (value) #:transparent)


(define (make-cf-grammar #:rules rules #:start-symbol start)
  (cf-grammar (sequence->vector rules) start))


(define (make-cf-production-rule #:symbol symbol #:substitution substitution #:label label)
  (cf-production-rule symbol label (sequence->vector substitution)))
