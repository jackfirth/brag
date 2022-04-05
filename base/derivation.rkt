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
    (-> semantic-action? parser-derivation? #:rest (listof parser-derivation?)
        nonterminal-derivation?))]
  [parser-derivation->syntax (-> parser-derivation? syntax?)]
  [parser-derivation->datum (-> parser-derivation? any/c)]
  [semantic-action? predicate/c]
  [cut-action cut-action?]
  [cut-action? predicate/c]
  [splice-action splice-action?]
  [splice-action? predicate/c]
  [label-action? predicate/c]
  [label-action (->* (any/c) (#:properties hash? #:expression-properties hash?) label-action?)]
  [label-action-value (-> label-action? any/c)]
  [label-action-properties (-> label-action? hash?)]
  [label-action-expression-properties (-> label-action? hash?)]))


(require racket/match
         racket/sequence
         racket/struct
         rebellion/collection/vector
         rebellion/private/static-name
         yaragg/base/token)


(module+ test
  (require (submod "..")
           racket/syntax-srcloc
           rackunit))


;@----------------------------------------------------------------------------------------------------


(define (semantic-action? v)
  (or (cut-action? v) (splice-action? v) (label-action? v)))


(struct cut-action () #:transparent #:constructor-name constructor:cut-action #:omit-define-syntaxes)
(define cut-action (constructor:cut-action))


(struct splice-action ()
  #:transparent #:constructor-name constructor:splice-action #:omit-define-syntaxes)
(define splice-action (constructor:splice-action))


(struct label-action (value expression-properties properties)
  #:transparent
  #:constructor-name constructor:label-action
  #:omit-define-syntaxes
  #:guard
  (struct-guard/c any/c
                  (hash/c any/c any/c #:immutable #true #:flat? #true)
                  (hash/c any/c any/c #:immutable #true #:flat? #true)))


(define (label-action value
                      #:properties [properties (hash)]
                      #:expression-properties [expression-properties (hash)])
  (constructor:label-action value properties expression-properties))


(define (parser-derivation? v)
  (or (terminal-derivation? v) (nonterminal-derivation? v)))


;; A (Terminal-Derivation V) represents a terminal that was matched by the grammar. It contains the
;; value V of the (Token T V) that was matched.
(struct terminal-derivation (value) #:transparent)


;; A (Nonterminal-Derivation V A) represents a nonterminal that was matched by the grammar. It
;; contains the action of type (Semantic-Action A) of the production rule that matched, and an
;; immutable vector of subderivations 
(struct nonterminal-derivation (action children)

  #:guard
  (let ([contract-guard
         (struct-guard/c semantic-action? (sequence/c parser-derivation? #:min-count 1))])
    (λ (action children name)
      (let-values ([(action children) (contract-guard action children name)])
        (values action (sequence->vector children)))))
  
  #:transparent
  #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (_) 'nonterminal-derivation)
      (λ (this)
        (cons (nonterminal-derivation-action this)
              (vector->list (nonterminal-derivation-children this))))))])


(define parser-derivation
  (case-lambda
    [(value) (terminal-derivation value)]
    [(action first-child . children) (nonterminal-derivation action (cons first-child children))]))


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
         (label-action 'a)
         (parser-derivation 1)
         (parser-derivation 2)
         (parser-derivation 3)))
      (check-equal? (parser-derivation-first-terminal derivation) 1))

    (test-case "nonterminal of nonterminals and terminals"
      (define derivation
        (parser-derivation
         (label-action 'a)
         (parser-derivation (label-action 'b) (parser-derivation 1))
         (parser-derivation 2)
         (parser-derivation 3)))
      (check-equal? (parser-derivation-first-terminal derivation) 1)))

  (test-case (name-string parser-derivation-last-terminal)

    (test-case "terminal"
      (check-equal? (parser-derivation-last-terminal (terminal-derivation 1)) 1))

    (test-case "nonterminal of terminals"
      (define derivation
        (parser-derivation
         (label-action 'a)
         (parser-derivation 1)
         (parser-derivation 2)
         (parser-derivation 3)))
      (check-equal? (parser-derivation-last-terminal derivation) 3))

    (test-case "nonterminal of nonterminals and terminals"
      (define derivation
        (parser-derivation
         (label-action 'a)
         (parser-derivation 1)
         (parser-derivation 2)
         (parser-derivation (label-action 'b) (parser-derivation 3))))
      (check-equal? (parser-derivation-last-terminal derivation) 3))))


(define (parser-derivation->syntax derivation)
  (define (->splice derivation)
    (match derivation
      [(terminal-derivation t) (list (syntax-token->syntax t))]
      [(nonterminal-derivation (? cut-action?) _) '()]
      [(nonterminal-derivation (? splice-action?) children)
       (for*/list ([child (in-vector children)]
                   [stx (in-list (->splice child))])
         stx)]
      [(nonterminal-derivation (? label-action? label) children)
       (define first-token (parser-derivation-first-terminal derivation))
       (define label-location
         (srcloc (syntax-token-source first-token)
                 (syntax-token-line first-token)
                 (syntax-token-column first-token)
                 (syntax-token-position first-token)
                 0))
       (define label-stx
         (for/fold ([stx (datum->syntax #false (label-action-value label) label-location #false)])
                   ([(key value) (in-hash (label-action-properties label))])
           (syntax-property stx key value)))
       (define children-syntaxes
         (for*/list ([child (in-vector children)]
                     [spliced-child (in-list (->splice child))])
           spliced-child))
       (define last-token (parser-derivation-last-terminal derivation))
       (define expression-location
         (srcloc (syntax-token-source first-token)
                 (syntax-token-line first-token)
                 (syntax-token-column first-token)
                 (syntax-token-position first-token)
                 (- (syntax-token-end-position last-token) (syntax-token-position first-token))))
       (define expression-stx
         (datum->syntax #false (cons label-stx children-syntaxes) expression-location #false))
       (list (for/fold ([expression-stx expression-stx])
                       ([(key value) (in-hash (label-action-expression-properties label))])
               (syntax-property expression-stx key value)))]))
  (define first-token (parser-derivation-first-terminal derivation))
  (define last-token (parser-derivation-last-terminal derivation))
  (define top-level-location
    (srcloc (syntax-token-source first-token)
            (syntax-token-line first-token)
            (syntax-token-column first-token)
            (syntax-token-position first-token)
            (- (syntax-token-end-position last-token) (syntax-token-position first-token))))
  (define top-level-stxs (->splice derivation))
  (match top-level-stxs
    [(list stx) stx]))


(define (parser-derivation->datum derivation)
  (define (->splice derivation)
    (match derivation
      [(terminal-derivation t) (list t)]
      [(nonterminal-derivation (? cut-action?) _) '()]
      [(nonterminal-derivation (? splice-action?) children)
       (for*/list ([child (in-vector children)]
                   [datum (in-list (->splice child))])
         datum)]
      [(nonterminal-derivation (? label-action? label) children)
       (define value (label-action-value label))
       (define child-data
         (for*/list ([child (in-vector children)]
                     [spliced-child (in-list (->splice child))])
           spliced-child))
       (list (cons value child-data))]))
  (define top-level-data (->splice derivation))
  (match top-level-data
    [(list datum) datum]))


(module+ test
  (test-case (name-string parser-derivation->datum)

    (test-case "datum terminals"
      (define derivation (parser-derivation 'a))
      (check-equal? (parser-derivation->datum derivation) 'a))

    (test-case "datum nonterminals"
      (define derivation
        (parser-derivation (label-action 'a)
                           (parser-derivation 'b)
                           (parser-derivation 'c)
                           (parser-derivation 'd)))
      (check-equal? (parser-derivation->datum derivation) '(a b c d)))

    (test-case "datum cuts"
      (define derivation
        (parser-derivation (label-action 'a)
                           (parser-derivation cut-action (parser-derivation 'b))
                           (parser-derivation 'c)
                           (parser-derivation cut-action (parser-derivation 'd))))
      (check-equal? (parser-derivation->datum derivation) '(a c)))

    (test-case "datum splices"
      (define derivation
        (parser-derivation (label-action 'a)
                           (parser-derivation 'b)
                           (parser-derivation splice-action
                                              (parser-derivation 'c1)
                                              (parser-derivation 'c2)
                                              (parser-derivation 'c3))
                           (parser-derivation 'd)))
      (check-equal? (parser-derivation->datum derivation) '(a b c1 c2 c3 d))))

  (test-case (name-string parser-derivation->syntax)

    (test-case "syntax terminals"
      (define derivation (parser-derivation (syntax-token 'a #:position 1 #:span 1)))
      (define actual (parser-derivation->syntax derivation))
      (check-equal? (syntax->datum actual) 'a)
      (check-equal? (syntax-srcloc actual) (srcloc #false #false #false 1 1)))

    (test-case "syntax nonterminals"
      (define derivation
        (parser-derivation
         (label-action 'a)
         (parser-derivation (syntax-token 'b #:position 1 #:span 1))
         (parser-derivation (syntax-token 'c #:position 2 #:span 1))
         (parser-derivation (syntax-token 'd #:position 3 #:span 1))))
      (define actual (parser-derivation->syntax derivation))
      (check-equal? (syntax->datum actual) '(a b c d))
      (check-equal? (syntax-srcloc actual) (srcloc #false #false #false 1 3)))

    (test-case "syntax cuts"
      (define derivation
        (parser-derivation
         (label-action 'a)
         (parser-derivation cut-action (parser-derivation (syntax-token 'b #:position 1 #:span 1)))
         (parser-derivation (syntax-token 'c #:position 2 #:span 1))
         (parser-derivation cut-action (parser-derivation (syntax-token 'd #:position 3 #:span 1)))))
      (define actual (parser-derivation->syntax derivation))
      (check-equal? (syntax->datum actual) '(a c))
      (check-equal? (syntax-srcloc actual) (srcloc #false #false #false 1 3)))

    (test-case "syntax splices"
      (define derivation
        (parser-derivation
         (label-action 'a)
         (parser-derivation (syntax-token 'b #:position 1 #:span 1))
         (parser-derivation splice-action
                            (parser-derivation (syntax-token 'c1 #:position 2 #:span 1))
                            (parser-derivation (syntax-token 'c2 #:position 3 #:span 1))
                            (parser-derivation (syntax-token 'c3 #:position 4 #:span 1)))
         (parser-derivation (syntax-token 'd #:position 5 #:span 1))))
      (define actual (parser-derivation->syntax derivation))
      (check-equal? (syntax->datum actual) '(a b c1 c2 c3 d))
      (check-equal? (syntax-srcloc actual) (srcloc #false #false #false 1 5)))))
