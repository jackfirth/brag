#lang racket/base


(require racket/contract/base)


(provide
 (struct-out cf-grammar)
 (struct-out cf-production-rule)
 (contract-out
  [cf-grammar-start-rules (-> cf-grammar? (set/c cf-production-rule? #:kind 'immutable))]
  [make-cf-grammar (-> #:rules (sequence/c cf-production-rule?) #:start-symbol any/c cf-grammar?)]
  [make-cf-production-rule
   (-> #:nonterminal any/c #:substitution (sequence/c grammar-symbol?) #:action semantic-action?
       cf-production-rule?)]))


(require racket/sequence
         racket/set
         rebellion/collection/vector
         yaragg/base/derivation
         yaragg/base/semantic-action
         yaragg/base/symbol)


;@----------------------------------------------------------------------------------------------------


;; Parsing takes a (Grammar T S A) and a sequence of (Token T V) and produces a set of
;; (Parser-Derivation V A) (also called a "parse forest"). A grammar contains an immutable
;; vector of (Context-Free-Production-Rule T S A) and a start symbol of type S.
;;   T: the terminals the grammar parses. Corresponds to the type field of the input tokens.
;;   S: the nonterminals the grammar rules are defined in terms of.
;;   A: the labels that grammar rules may have attached to them via the (Label-Action A) semantic
;;      action. These show up in parse tree branches, and can be used to determine which production
;;      rule produced a derivation.
(struct cf-grammar (rules start-symbol) #:transparent)


(define (cf-grammar-start-rules grammar)
  (define start (cf-grammar-start-symbol grammar))
  (for/set ([rule (in-vector (cf-grammar-rules grammar))]
            #:when (equal? (cf-production-rule-nonterminal rule) start))
    rule))


;; A (Context-Free-Production-Rule T S A) contains a nonterminal symbol of type S, semantic action of
;; type (Semnatic-Action A), and a substitution sequence of (Grammar-Symbol T S) values, stored in an
;; immutable vector.
(struct cf-production-rule (nonterminal action substitution) #:transparent)


(define (make-cf-grammar #:rules rules #:start-symbol start)
  (cf-grammar (sequence->vector rules) start))


(define (make-cf-production-rule #:nonterminal nonterminal
                                 #:action action
                                 #:substitution substitution)
  (cf-production-rule nonterminal action (sequence->vector substitution)))
