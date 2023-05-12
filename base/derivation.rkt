#lang racket/base


(require racket/contract/base)


(provide
 (struct-out terminal-derivation)
 (struct-out nonterminal-derivation)
 (contract-out
  [parser-derivation? predicate/c]
  [parser-derivation
   (case->
    (-> token? terminal-derivation?)
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
           racket/list
           racket/syntax-srcloc
           rackunit
           syntax/parse))


;@----------------------------------------------------------------------------------------------------


(struct parser-derivation ()
  #:transparent
  #:name type:parser-derivation
  #:constructor-name constructor:parser-derivation)


;; Tepresents a terminal that was matched by the grammar. It contains the token or lexeme that was
;; matched.
(struct terminal-derivation type:parser-derivation (matched) #:transparent)


;; A (Nonterminal-Derivation V A) represents a nonterminal that was matched by the grammar. It
;; contains the action of type (Semantic-Action A) of the production rule that matched, and an
;; immutable vector of subderivations 
(struct nonterminal-derivation type:parser-derivation (action children)

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
    [(atom) (terminal-derivation atom)]
    [(action first-child . children) (nonterminal-derivation action (cons first-child children))]))


(define (parser-derivation-first-token derivation)
  (match derivation
    [(terminal-derivation atom) atom]
    [(nonterminal-derivation _ (vector first-child _ ...))
     (parser-derivation-first-token first-child)]))


(define (parser-derivation-last-token derivation)
  (match derivation
    [(terminal-derivation atom) atom]
    [(nonterminal-derivation _ (vector _ ... last-child))
     (parser-derivation-last-token last-child)]))


(module+ test
  (define one (atom 'number 1))
  (define two (atom 'number 2))
  (define three (atom 'number 3))

  (test-case (name-string parser-derivation-first-token)
    
    (test-case "terminal"
      (check-equal? (parser-derivation-first-token (parser-derivation one)) one))

    (test-case "nonterminal of terminals"
      (define derivation
        (parser-derivation
         (label-action 'a)
         (parser-derivation one)
         (parser-derivation two)
         (parser-derivation three)))
      (check-equal? (parser-derivation-first-token derivation) one))

    (test-case "nonterminal of nonterminals and terminals"
      (define derivation
        (parser-derivation
         (label-action 'a)
         (parser-derivation (label-action 'b) (parser-derivation one))
         (parser-derivation two)
         (parser-derivation three)))
      (check-equal? (parser-derivation-first-token derivation) one)))

  (test-case (name-string parser-derivation-last-token)

    (test-case "terminal"
      (check-equal? (parser-derivation-last-token (terminal-derivation one)) one))

    (test-case "nonterminal of terminals"
      (define derivation
        (parser-derivation
         (label-action 'a)
         (parser-derivation one)
         (parser-derivation two)
         (parser-derivation three)))
      (check-equal? (parser-derivation-last-token derivation) three))

    (test-case "nonterminal of nonterminals and terminals"
      (define derivation
        (parser-derivation
         (label-action 'a)
         (parser-derivation one)
         (parser-derivation two)
         (parser-derivation (label-action 'b) (parser-derivation three))))
      (check-equal? (parser-derivation-last-token derivation) three))))


(define (parser-derivation->syntax derivation)
  
  (define (->splice derivation)
    (define first-token (parser-derivation-first-token derivation))
    (define last-token (parser-derivation-last-token derivation))
    (match derivation
      [(terminal-derivation (? atom? a)) (list (atom->syntax a))]
      [(terminal-derivation _) (list)]
      [(nonterminal-derivation action children)
       (define children-syntaxes
         (for*/list ([child (in-vector children)]
                     [spliced-child (in-list (->splice child))])
           spliced-child))
       (semantic-action-build-syntax-splice
        action children-syntaxes
        #:first-location (token-location first-token)
        #:last-location (token-location last-token))]))
  
  (match (->splice derivation)
    [(list stx) stx]))


(define (parser-derivation->datum derivation)

  (define (->splice derivation)
    (match derivation
      [(terminal-derivation (? atom? a)) (list (atom-datum a))]
      [(terminal-derivation _) (list)]
      [(nonterminal-derivation action children)
       (define child-data
         (for*/list ([child (in-vector children)]
                     [datum (in-list (->splice child))])
           datum))
       (semantic-action-build-datum-splice action child-data)]))
  
  (match (->splice derivation)
    [(list datum) datum]))


(module+ test

  (define (identifier name #:position [position #false] #:span [span #false])
    (atom 'identifier name #:position position #:span span))
  
  (test-case (name-string parser-derivation->datum)

    (test-case "datum terminals"
      (define derivation (parser-derivation (identifier 'a)))
      (check-equal? (parser-derivation->datum derivation) 'a))

    (test-case "datum labels"
      (define derivation
        (parser-derivation (label-action 'a)
                           (parser-derivation (identifier 'b))
                           (parser-derivation (identifier 'c))
                           (parser-derivation (identifier 'd))))
      (check-equal? (parser-derivation->datum derivation) '(a b c d)))

    (test-case "datum cuts"
      (define derivation
        (parser-derivation (label-action 'a)
                           (parser-derivation cut-action (parser-derivation (identifier 'b)))
                           (parser-derivation (identifier 'c))
                           (parser-derivation cut-action (parser-derivation (identifier 'd)))))
      (check-equal? (parser-derivation->datum derivation) '(a c)))

    (test-case "datum splices"
      (define derivation
        (parser-derivation (label-action 'a)
                           (parser-derivation (identifier 'b))
                           (parser-derivation splice-action
                                              (parser-derivation (identifier 'c1))
                                              (parser-derivation (identifier 'c2))
                                              (parser-derivation (identifier 'c3)))
                           (parser-derivation (identifier 'd))))
      (check-equal? (parser-derivation->datum derivation) '(a b c1 c2 c3 d)))

    (test-case "datum pairs"
      (define derivation
        (parser-derivation
         (build-pair-action) (parser-derivation (identifier 'b)) (parser-derivation (identifier 'c))))
      (check-equal? (parser-derivation->datum derivation) '(b . c)))

    (test-case "datum lists"
      (define derivation
        (parser-derivation (build-list-action)
                           (parser-derivation (identifier 'b))
                           (parser-derivation (identifier 'c))
                           (parser-derivation (identifier 'd))))
      (check-equal? (parser-derivation->datum derivation) '(b c d)))

    (test-case "improper datum lists"
      (define derivation
        (parser-derivation (build-improper-list-action)
                           (parser-derivation (identifier 'b))
                           (parser-derivation (identifier 'c))
                           (parser-derivation (identifier 'd))))
      (check-equal? (parser-derivation->datum derivation) '(b c . d)))

    (test-case "datum vectors"
      (define derivation
        (parser-derivation (build-vector-action)
                           (parser-derivation (identifier 'b))
                           (parser-derivation (identifier 'c))
                           (parser-derivation (identifier 'd))))
      (check-equal? (parser-derivation->datum derivation) #(b c d)))

    (test-case "datum hashes"
      (define derivation
        (parser-derivation (build-hash-action)
                           (parser-derivation (identifier 'a))
                           (parser-derivation one)
                           (parser-derivation (identifier 'b))
                           (parser-derivation two)))
      (check-equal? (parser-derivation->datum derivation) (hash 'a 1 'b 2)))

    (test-case "datum hashes with duplicate keys"
      (define derivation
        (parser-derivation (build-hash-action)
                           (parser-derivation (identifier 'a))
                           (parser-derivation one)
                           (parser-derivation (identifier 'a))
                           (parser-derivation two)))
      (check-exn exn:fail:contract? (λ () (parser-derivation->datum derivation))))

    (test-case "datum boxes"
      (define derivation
        (parser-derivation (build-box-action) (parser-derivation (identifier 'a))))
      (check-equal? (parser-derivation->datum derivation) (box-immutable 'a)))

    (test-case "prefab datum structs"
      (define derivation
        (parser-derivation (build-prefab-struct-action 'point)
                           (parser-derivation one)
                           (parser-derivation two)))
      (check-equal? (parser-derivation->datum derivation) #s(point 1 2))))

  (test-case (name-string parser-derivation->syntax)

    (test-case "syntax terminals"
      (define derivation (parser-derivation (identifier 'a #:position 1 #:span 1)))
      (define actual (parser-derivation->syntax derivation))
      (check-equal? (syntax->datum actual) 'a)
      (check-equal? (syntax-srcloc actual) (srcloc #false #false #false 1 1)))

    (test-case "syntax labels"
      (define derivation
        (parser-derivation
         (label-action 'a)
         (parser-derivation (identifier 'b #:position 1 #:span 1))
         (parser-derivation (identifier 'c #:position 2 #:span 1))
         (parser-derivation (identifier 'd #:position 3 #:span 1))))
      (define actual (parser-derivation->syntax derivation))
      (check-equal? (syntax->datum actual) '(a b c d))
      (check-equal? (syntax-srcloc actual) (srcloc #false #false #false 1 3))
      (check-equal? (syntax-srcloc (first (syntax-e actual))) (srcloc #false #false #false 1 0))
      (check-equal? (syntax-srcloc (second (syntax-e actual))) (srcloc #false #false #false 1 1))
      (check-equal? (syntax-srcloc (third (syntax-e actual))) (srcloc #false #false #false 2 1))
      (check-equal? (syntax-srcloc (fourth (syntax-e actual))) (srcloc #false #false #false 3 1)))

    (test-case "nested syntax labels"
      (define derivation
        (parser-derivation
         (label-action 'outer)
         (parser-derivation (identifier 'a #:position 1 #:span 1))
         (parser-derivation
          (label-action 'inner)
          (parser-derivation (identifier 'b #:position 2 #:span 1))
          (parser-derivation (identifier 'c #:position 3 #:span 1)))
         (parser-derivation (identifier 'd #:position 4 #:span 1))))
      (define actual (parser-derivation->syntax derivation))
      (check-equal? (syntax->datum actual) '(outer a (inner b c) d))
      (check-equal? (syntax-srcloc actual) (srcloc #false #false #false 1 4))
      (match-define (list outer a inner-expr d) (syntax-e actual))
      (match-define (list inner b c) (syntax-e inner-expr))
      (check-equal? (syntax-srcloc outer) (srcloc #false #false #false 1 0))
      (check-equal? (syntax-srcloc a) (srcloc #false #false #false 1 1))
      (check-equal? (syntax-srcloc inner-expr) (srcloc #false #false #false 2 2))
      (check-equal? (syntax-srcloc inner) (srcloc #false #false #false 2 0))
      (check-equal? (syntax-srcloc b) (srcloc #false #false #false 2 1))
      (check-equal? (syntax-srcloc c) (srcloc #false #false #false 3 1))
      (check-equal? (syntax-srcloc d) (srcloc #false #false #false 4 1)))

    (test-case "syntax cuts"
      (define derivation
        (parser-derivation
         (label-action 'a)
         (parser-derivation cut-action (parser-derivation (identifier 'b #:position 1 #:span 1)))
         (parser-derivation (identifier 'c #:position 2 #:span 1))
         (parser-derivation cut-action (parser-derivation (identifier 'd #:position 3 #:span 1)))))
      (define actual (parser-derivation->syntax derivation))
      (check-equal? (syntax->datum actual) '(a c))
      (check-equal? (syntax-srcloc actual) (srcloc #false #false #false 1 3))
      (check-equal? (syntax-srcloc (first (syntax-e actual))) (srcloc #false #false #false 1 0))
      (check-equal? (syntax-srcloc (second (syntax-e actual))) (srcloc #false #false #false 2 1)))

    (test-case "syntax splices"
      (define derivation
        (parser-derivation
         (label-action 'a)
         (parser-derivation (identifier 'b #:position 1 #:span 1))
         (parser-derivation splice-action
                            (parser-derivation (identifier 'c1 #:position 2 #:span 1))
                            (parser-derivation (identifier 'c2 #:position 3 #:span 1))
                            (parser-derivation (identifier 'c3 #:position 4 #:span 1)))
         (parser-derivation (identifier 'd #:position 5 #:span 1))))
      (define actual (parser-derivation->syntax derivation))
      (check-equal? (syntax->datum actual) '(a b c1 c2 c3 d))
      (check-equal? (syntax-srcloc actual) (srcloc #false #false #false 1 5)))

    (test-case "syntax pairs"
      (define derivation
        (parser-derivation
         (build-pair-action)
         (parser-derivation (identifier 'b #:position 1 #:span 1))
         (parser-derivation (identifier 'c #:position 2 #:span 1))))
      (define actual (parser-derivation->syntax derivation))
      (check-equal? (syntax->datum actual) '(b . c))
      (check-equal? (syntax-srcloc actual) (srcloc #false #false #false 1 2)))

    (test-case "syntax lists"
      (define derivation
        (parser-derivation
         (build-list-action)
         (parser-derivation (identifier 'b #:position 1 #:span 1))
         (parser-derivation (identifier 'c #:position 2 #:span 1))
         (parser-derivation (identifier 'd #:position 3 #:span 1))))
      (define actual (parser-derivation->syntax derivation))
      (check-equal? (syntax->datum actual) '(b c d))
      (check-equal? (syntax-srcloc actual) (srcloc #false #false #false 1 3)))

    (test-case "improper syntax lists"
      (define derivation
        (parser-derivation
         (build-improper-list-action)
         (parser-derivation (identifier 'b #:position 1 #:span 1))
         (parser-derivation (identifier 'c #:position 2 #:span 1))
         (parser-derivation (identifier 'd #:position 3 #:span 1))))
      (define actual (parser-derivation->syntax derivation))
      (check-equal? (syntax->datum actual) '(b c . d))
      (check-equal? (syntax-srcloc actual) (srcloc #false #false #false 1 3)))

    (test-case "syntax vectors"
      (define derivation
        (parser-derivation
         (build-vector-action)
         (parser-derivation (identifier 'b #:position 1 #:span 1))
         (parser-derivation (identifier 'c #:position 2 #:span 1))
         (parser-derivation (identifier 'd #:position 3 #:span 1))))
      (define actual (parser-derivation->syntax derivation))
      (check-equal? (syntax->datum actual) #(b c d))
      (check-equal? (syntax-srcloc actual) (srcloc #false #false #false 1 3)))

    (test-case "syntax hash tables"
      (define derivation
        (parser-derivation
         (build-hash-action)
         (parser-derivation (identifier 'a #:position 1 #:span 1))
         (parser-derivation (identifier 1 #:position 2 #:span 1))
         (parser-derivation (identifier 'b #:position 3 #:span 1))
         (parser-derivation (identifier 2 #:position 4 #:span 1))))
      (define actual (parser-derivation->syntax derivation))
      (check-equal? (syntax->datum actual) (hash 'a 1 'b 2))
      (check-equal? (syntax-srcloc actual) (srcloc #false #false #false 1 4)))

    (test-case "syntax hash tables with duplicate keys"
      (define derivation
        (parser-derivation
         (build-hash-action)
         (parser-derivation (identifier 'a #:position 1 #:span 1))
         (parser-derivation (identifier 1 #:position 2 #:span 1))
         (parser-derivation (identifier 'a #:position 3 #:span 1))
         (parser-derivation (identifier 2 #:position 4 #:span 1))))
      (check-exn exn:fail:contract? (λ () (parser-derivation->syntax derivation))))

    (test-case "syntax boxes"
      (define derivation
        (parser-derivation
         (build-box-action) (parser-derivation (identifier 'a #:position 1 #:span 1))))
      (define actual (parser-derivation->syntax derivation))
      (check-equal? (syntax->datum actual) (box-immutable 'a))
      (check-equal? (syntax-srcloc actual) (srcloc #false #false #false 1 1)))

    (test-case "prefab syntax structs"
      (define derivation
        (parser-derivation
         (build-prefab-struct-action 'point)
         (parser-derivation (identifier 5 #:position 1 #:span 1))
         (parser-derivation (identifier 10 #:position 2 #:span 1))))
      (define actual (parser-derivation->syntax derivation))
      (check-equal? (syntax->datum actual) #s(point 5 10))
      (check-equal? (syntax-srcloc actual) (srcloc #false #false #false 1 2)))))
