#lang racket/base


(require racket/contract/base)


(provide
 (struct-out terminal-symbol)
 (struct-out nonterminal-symbol)
 (contract-out
  [grammar-symbol? predicate/c]))


;@----------------------------------------------------------------------------------------------------


(define (grammar-symbol? v)
  (or (terminal-symbol? v) (nonterminal-symbol? v)))


(struct terminal-symbol (value) #:transparent)
(struct nonterminal-symbol (value) #:transparent)
