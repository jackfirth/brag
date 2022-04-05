#lang racket/base


(require racket/contract/base)


(provide
 (struct-out terminal-derivation)
 (struct-out nonterminal-derivation)
 (struct-out syntax-label)
 (contract-out
  [parser-derivation? predicate/c]
  [parser-derivation-first-terminal (-> parser-derivation? any/c)]
  [parser-derivation-last-terminal (-> parser-derivation? any/c)]
  [parser-derivation
   (case->
    (-> any/c terminal-derivation?)
    (-> any/c parser-derivation? #:rest (listof parser-derivation?) nonterminal-derivation?))]
  [parser-derivation->syntax (-> parser-derivation? syntax?)]))


(require racket/match
         racket/sequence
         racket/struct
         rebellion/collection/vector
         yaragg/base/token)


;@----------------------------------------------------------------------------------------------------


(define (parser-derivation? v)
  (or (terminal-derivation? v) (nonterminal-derivation? v)))


;; A (Terminal-Derivation V) represents a terminal that was matched by the grammar. It contains the
;; value V of the (Token T V) that was matched.
(struct terminal-derivation (value) #:transparent)


;; A (Nonterminal-Derivation V L) represents a nonterminal that was matched by the grammar. It
;; contains the label of type L of the production rule that matched, and an immutable vector of
;; subderivations 
(struct nonterminal-derivation (label children)

  #:guard
  (let ([contract-guard (struct-guard/c any/c (sequence/c parser-derivation?))])
    (λ (label children name)
      (let-values ([(label children) (contract-guard label children name)])
        (values label (sequence->vector children)))))
  
  #:transparent
  #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (_) 'nonterminal-derivation)
      (λ (this)
        (cons (nonterminal-derivation-label this)
              (vector->list (nonterminal-derivation-children this))))))])


(define parser-derivation
  (case-lambda
    [(value) (terminal-derivation value)]
    [(label first-child . children) (nonterminal-derivation label (cons first-child children))]))


(define (parser-derivation-first-terminal derivation)
  (match derivation
    [(terminal-derivation value) value]
    [(nonterminal-derivation _ (list first-child _ ...))
     (parser-derivation-first-terminal first-child)]))


(define (parser-derivation-last-terminal derivation)
  (match derivation
    [(terminal-derivation value) value]
    [(nonterminal-derivation _ (list _ ... last-child))
     (parser-derivation-first-terminal last-child)]))


(struct syntax-label (value expression-properties properties)
  #:transparent
  #:guard
  (struct-guard/c any/c
                  (hash/c any/c any/c #:immutable #true #:flat? #true)
                  (hash/c any/c any/c #:immutable #true #:flat? #true)))


(define (parser-derivation->syntax derivation)
  (match derivation
    [(terminal-derivation t) (syntax-token->syntax t)]
    [(nonterminal-derivation label children)
     (define first-token (parser-derivation-first-terminal derivation))
     (define last-token (parser-derivation-last-terminal derivation))
     (define location
       (srcloc (syntax-token-source first-token)
               (syntax-token-line first-token)
               (syntax-token-column first-token)
               (syntax-token-position first-token)
               (- (syntax-token-position first-token) (syntax-token-end-position last-token))))
     (define label-location
       (srcloc (syntax-token-source first-token)
               (syntax-token-line first-token)
               (syntax-token-column first-token)
               (syntax-token-position first-token)
               0))
     (define label-stx
       (for/fold ([stx (datum->syntax #false (syntax-label-value label) label-location #false)])
                 ([(key value) (in-hash (syntax-label-properties label))])
         (syntax-property stx key value)))
     (for/fold ([stx (datum->syntax #false (cons label-stx children) location #false)])
               ([(key value) (in-hash (syntax-label-expression-properties label))])
       (syntax-property stx key value))]))