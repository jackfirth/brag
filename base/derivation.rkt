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
  [parser-derivation->datum (-> parser-derivation? any/c)]))


(require racket/match
         racket/sequence
         racket/struct
         rebellion/collection/vector
         rebellion/private/static-name
         yaragg/base/semantic-action
         yaragg/base/token)


(module+ test
  (require (submod "..")
           racket/syntax-srcloc
           rackunit))


;@----------------------------------------------------------------------------------------------------


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
  (define first-token (parser-derivation-first-terminal derivation))
  (define last-token (parser-derivation-last-terminal derivation))

  (define (->splice derivation)
    (match derivation
      [(terminal-derivation t) (list (syntax-token->syntax t))]
      [(nonterminal-derivation action children)
       (define children-syntaxes
         (for*/list ([child (in-vector children)]
                     [spliced-child (in-list (->splice child))])
           spliced-child))
       (semantic-action-build-syntax-splice
        action children-syntaxes #:first-token first-token #:last-token last-token)]))
  
  (match (->splice derivation)
    [(list stx) stx]))


(define (parser-derivation->datum derivation)

  (define (->splice derivation)
    (match derivation
      [(terminal-derivation t) (list t)]
      [(nonterminal-derivation action children)
       (define child-data
         (for*/list ([child (in-vector children)]
                     [datum (in-list (->splice child))])
           datum))
       (semantic-action-build-datum-splice action child-data)]))
  
  (match (->splice derivation)
    [(list datum) datum]))


(module+ test
  (test-case (name-string parser-derivation->datum)

    (test-case "datum terminals"
      (define derivation (parser-derivation 'a))
      (check-equal? (parser-derivation->datum derivation) 'a))

    (test-case "datum labels"
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
      (check-equal? (parser-derivation->datum derivation) '(a b c1 c2 c3 d)))

    (test-case "datum pairs"
      (define derivation
        (parser-derivation (build-pair-action) (parser-derivation 'b) (parser-derivation 'c)))
      (check-equal? (parser-derivation->datum derivation) '(b . c)))

    (test-case "datum lists"
      (define derivation
        (parser-derivation (build-list-action)
                           (parser-derivation 'b)
                           (parser-derivation 'c)
                           (parser-derivation 'd)))
      (check-equal? (parser-derivation->datum derivation) '(b c d)))

    (test-case "improper datum lists"
      (define derivation
        (parser-derivation (build-improper-list-action)
                           (parser-derivation 'b)
                           (parser-derivation 'c)
                           (parser-derivation 'd)))
      (check-equal? (parser-derivation->datum derivation) '(b c . d)))

    (test-case "datum vectors"
      (define derivation
        (parser-derivation (build-vector-action)
                           (parser-derivation 'b)
                           (parser-derivation 'c)
                           (parser-derivation 'd)))
      (check-equal? (parser-derivation->datum derivation) #(b c d)))

    (test-case "datum hashes"
      (define derivation
        (parser-derivation (build-hash-action)
                           (parser-derivation 'a)
                           (parser-derivation 1)
                           (parser-derivation 'b)
                           (parser-derivation 2)))
      (check-equal? (parser-derivation->datum derivation) (hash 'a 1 'b 2)))

    (test-case "datum hashes with duplicate keys"
      (define derivation
        (parser-derivation (build-hash-action)
                           (parser-derivation 'a)
                           (parser-derivation 1)
                           (parser-derivation 'a)
                           (parser-derivation 2)))
      (check-exn exn:fail:contract? (λ () (parser-derivation->datum derivation))))

    (test-case "datum boxes"
      (define derivation
        (parser-derivation (build-box-action) (parser-derivation 'a)))
      (check-equal? (parser-derivation->datum derivation) (box-immutable 'a)))

    (test-case "prefab datum structs"
      (define derivation
        (parser-derivation (build-prefab-struct-action 'point)
                           (parser-derivation 1)
                           (parser-derivation 2)))
      (check-equal? (parser-derivation->datum derivation) #s(point 1 2))))

  (test-case (name-string parser-derivation->syntax)

    (test-case "syntax terminals"
      (define derivation (parser-derivation (syntax-token 'a #:position 1 #:span 1)))
      (define actual (parser-derivation->syntax derivation))
      (check-equal? (syntax->datum actual) 'a)
      (check-equal? (syntax-srcloc actual) (srcloc #false #false #false 1 1)))

    (test-case "syntax labels"
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
      (check-equal? (syntax-srcloc actual) (srcloc #false #false #false 1 5)))

    (test-case "syntax pairs"
      (define derivation
        (parser-derivation
         (build-pair-action)
         (parser-derivation (syntax-token 'b #:position 1 #:span 1))
         (parser-derivation (syntax-token 'c #:position 2 #:span 1))))
      (define actual (parser-derivation->syntax derivation))
      (check-equal? (syntax->datum actual) '(b . c))
      (check-equal? (syntax-srcloc actual) (srcloc #false #false #false 1 2)))

    (test-case "syntax lists"
      (define derivation
        (parser-derivation
         (build-list-action)
         (parser-derivation (syntax-token 'b #:position 1 #:span 1))
         (parser-derivation (syntax-token 'c #:position 2 #:span 1))
         (parser-derivation (syntax-token 'd #:position 3 #:span 1))))
      (define actual (parser-derivation->syntax derivation))
      (check-equal? (syntax->datum actual) '(b c d))
      (check-equal? (syntax-srcloc actual) (srcloc #false #false #false 1 3)))

    (test-case "improper syntax lists"
      (define derivation
        (parser-derivation
         (build-improper-list-action)
         (parser-derivation (syntax-token 'b #:position 1 #:span 1))
         (parser-derivation (syntax-token 'c #:position 2 #:span 1))
         (parser-derivation (syntax-token 'd #:position 3 #:span 1))))
      (define actual (parser-derivation->syntax derivation))
      (check-equal? (syntax->datum actual) '(b c . d))
      (check-equal? (syntax-srcloc actual) (srcloc #false #false #false 1 3)))

    (test-case "syntax vectors"
      (define derivation
        (parser-derivation
         (build-vector-action)
         (parser-derivation (syntax-token 'b #:position 1 #:span 1))
         (parser-derivation (syntax-token 'c #:position 2 #:span 1))
         (parser-derivation (syntax-token 'd #:position 3 #:span 1))))
      (define actual (parser-derivation->syntax derivation))
      (check-equal? (syntax->datum actual) #(b c d))
      (check-equal? (syntax-srcloc actual) (srcloc #false #false #false 1 3)))

    (test-case "syntax hash tables"
      (define derivation
        (parser-derivation
         (build-hash-action)
         (parser-derivation (syntax-token 'a #:position 1 #:span 1))
         (parser-derivation (syntax-token 1 #:position 2 #:span 1))
         (parser-derivation (syntax-token 'b #:position 3 #:span 1))
         (parser-derivation (syntax-token 2 #:position 4 #:span 1))))
      (define actual (parser-derivation->syntax derivation))
      (check-equal? (syntax->datum actual) (hash 'a 1 'b 2))
      (check-equal? (syntax-srcloc actual) (srcloc #false #false #false 1 4)))

    (test-case "syntax hash tables with duplicate keys"
      (define derivation
        (parser-derivation
         (build-hash-action)
         (parser-derivation (syntax-token 'a #:position 1 #:span 1))
         (parser-derivation (syntax-token 1 #:position 2 #:span 1))
         (parser-derivation (syntax-token 'a #:position 3 #:span 1))
         (parser-derivation (syntax-token 2 #:position 4 #:span 1))))
      (check-exn exn:fail:contract? (λ () (parser-derivation->syntax derivation))))

    (test-case "syntax boxes"
      (define derivation
        (parser-derivation
         (build-box-action) (parser-derivation (syntax-token 'a #:position 1 #:span 1))))
      (define actual (parser-derivation->syntax derivation))
      (check-equal? (syntax->datum actual) (box-immutable 'a))
      (check-equal? (syntax-srcloc actual) (srcloc #false #false #false 1 1)))

    (test-case "prefab syntax structs"
      (define derivation
        (parser-derivation
         (build-prefab-struct-action 'point)
         (parser-derivation (syntax-token 5 #:position 1 #:span 1))
         (parser-derivation (syntax-token 10 #:position 2 #:span 1))))
      (define actual (parser-derivation->syntax derivation))
      (check-equal? (syntax->datum actual) #s(point 5 10))
      (check-equal? (syntax-srcloc actual) (srcloc #false #false #false 1 2)))))
