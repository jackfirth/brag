#lang racket/base


(require racket/contract/base)


(provide
 (struct-out terminal-derivation)
 (struct-out nonterminal-derivation)
 (contract-out
  [parser-derivation? predicate/c]
  [parser-derivation-first-terminal (-> parser-derivation? any/c)]
  [parser-derivation-last-terminal (-> parser-derivation? any/c)]
  [parser-derivation
   (case->
    (-> any/c terminal-derivation?)
    (-> any/c parser-derivation? #:rest (listof parser-derivation?) nonterminal-derivation?))]
  [parser-derivation->syntax (-> parser-derivation? syntax?)]
  [syntax-label? predicate/c]
  [syntax-label (->* (any/c) (#:properties hash? #:expression-properties hash?) syntax-label?)]
  [syntax-label-value (-> syntax-label? any/c)]
  [syntax-label-properties (-> syntax-label? hash?)]
  [syntax-label-expression-properties (-> syntax-label? hash?)]))


(require racket/match
         racket/sequence
         racket/struct
         rebellion/collection/vector
         rebellion/private/static-name
         yaragg/base/token)


(module+ test
  (require (submod "..")
           rackunit))


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
  (let ([contract-guard (struct-guard/c any/c (sequence/c parser-derivation? #:min-count 1))])
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
    [(nonterminal-derivation _ (vector first-child _ ...))
     (parser-derivation-first-terminal first-child)]))


(define (parser-derivation-last-terminal derivation)
  (match derivation
    [(terminal-derivation value) value]
    [(nonterminal-derivation _ (vector _ ... last-child))
     (parser-derivation-last-terminal last-child)]))


(module+ test
  (test-case (name-string parser-derivation-first-terminal)

    (test-case "terminal"
      (check-equal? (parser-derivation-first-terminal (terminal-derivation 1)) 1))

    (test-case "nonterminal of terminals"
      (define derivation
        (parser-derivation
         'a
         (parser-derivation 1)
         (parser-derivation 2)
         (parser-derivation 3)))
      (check-equal? (parser-derivation-first-terminal derivation) 1))

    (test-case "nonterminal of nonterminals and terminals"
      (define derivation
        (parser-derivation
         'a
         (parser-derivation 'b (parser-derivation 1))
         (parser-derivation 2)
         (parser-derivation 3)))
      (check-equal? (parser-derivation-first-terminal derivation) 1)))

  (test-case (name-string parser-derivation-last-terminal)

    (test-case "terminal"
      (check-equal? (parser-derivation-last-terminal (terminal-derivation 1)) 1))

    (test-case "nonterminal of terminals"
      (define derivation
        (parser-derivation
         'a
         (parser-derivation 1)
         (parser-derivation 2)
         (parser-derivation 3)))
      (check-equal? (parser-derivation-last-terminal derivation) 3))

    (test-case "nonterminal of nonterminals and terminals"
      (define derivation
        (parser-derivation
         'a
         (parser-derivation 1)
         (parser-derivation 2)
         (parser-derivation 'b (parser-derivation 3))))
      (check-equal? (parser-derivation-last-terminal derivation) 3))))


(struct syntax-label (value expression-properties properties)
  #:transparent
  #:constructor-name constructor:syntax-label
  #:omit-define-syntaxes
  #:guard
  (struct-guard/c any/c
                  (hash/c any/c any/c #:immutable #true #:flat? #true)
                  (hash/c any/c any/c #:immutable #true #:flat? #true)))


(define (syntax-label value
                      #:properties [properties (hash)]
                      #:expression-properties [expression-properties (hash)])
  (constructor:syntax-label value properties expression-properties))


(define (parser-derivation->syntax derivation)
  (match derivation
    [(terminal-derivation t) (syntax-token->syntax t)]
    [(nonterminal-derivation label children)
     (define first-token (parser-derivation-first-terminal derivation))
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
     (define children-syntaxes
       (for/list ([child (in-vector children)])
         (parser-derivation->syntax child)))
     (define last-token (parser-derivation-last-terminal derivation))
     (define expression-location
       (srcloc (syntax-token-source first-token)
               (syntax-token-line first-token)
               (syntax-token-column first-token)
               (syntax-token-position first-token)
               (- (syntax-token-end-position last-token) (syntax-token-position first-token))))
     (define expression-stx
       (datum->syntax #false (cons label-stx children-syntaxes) expression-location #false))
     (for/fold ([expression-stx expression-stx])
               ([(key value) (in-hash (syntax-label-expression-properties label))])
       (syntax-property expression-stx key value))]))
