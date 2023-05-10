#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [semantic-action? predicate/c]
  [semantic-action-build-datum-splice (-> semantic-action? list? list?)]
  [semantic-action-build-syntax-splice
   (-> semantic-action? (listof syntax?) #:first-location srcloc? #:last-location srcloc?
       (listof syntax?))]
  [cut-action cut-action?]
  [cut-action? predicate/c]
  [splice-action splice-action?]
  [splice-action? predicate/c]
  [label-action? predicate/c]
  [label-action (->* (any/c) (#:properties hash? #:expression-properties hash?) label-action?)]
  [label-action-value (-> label-action? any/c)]
  [label-action-properties (-> label-action? hash?)]
  [label-action-expression-properties (-> label-action? hash?)]
  [build-pair-action (->* () (#:properties hash?) build-pair-action?)]
  [build-pair-action? predicate/c]
  [build-list-action (->* () (#:properties hash?) build-list-action?)]
  [build-list-action? predicate/c]
  [build-improper-list-action (->* () (#:properties hash?) build-improper-list-action?)]
  [build-improper-list-action? predicate/c]
  [build-vector-action (->* () (#:properties hash?) build-vector-action?)]
  [build-vector-action? predicate/c]
  [build-hash-action (->* () (#:kind (or/c 'equal 'eqv 'eq) #:properties hash?) build-hash-action?)]
  [build-hash-action? predicate/c]
  [build-box-action (->* () (#:properties hash?) build-box-action?)]
  [build-box-action? predicate/c]
  [build-prefab-struct-action (->* (prefab-key?) (#:properties hash?) build-prefab-struct-action?)]
  [build-prefab-struct-action? predicate/c]))


(require racket/list
         racket/match
         racket/sequence
         rebellion/private/static-name
         yaragg/base/token)


;@----------------------------------------------------------------------------------------------------


(define (semantic-action? v)
  (or (cut-action? v)
      (splice-action? v)
      (label-action? v)
      (build-pair-action? v)
      (build-list-action? v)
      (build-improper-list-action? v)
      (build-vector-action? v)
      (build-hash-action? v)
      (build-box-action? v)
      (build-prefab-struct-action? v)))


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


(struct build-pair-action (properties)
  #:transparent
  #:constructor-name constructor:build-pair-action
  #:omit-define-syntaxes)


(define (build-pair-action #:properties [properties (hash)])
  (constructor:build-pair-action properties))


(struct build-list-action (properties)
  #:transparent
  #:constructor-name constructor:build-list-action
  #:omit-define-syntaxes)


(define (build-list-action #:properties [properties (hash)])
  (constructor:build-list-action properties))


(struct build-improper-list-action (properties)
  #:transparent
  #:constructor-name constructor:build-improper-list-action
  #:omit-define-syntaxes)


(define (build-improper-list-action #:properties [properties (hash)])
  (constructor:build-improper-list-action properties))


(struct build-vector-action (properties)
  #:transparent
  #:constructor-name constructor:build-vector-action
  #:omit-define-syntaxes)


(define (build-vector-action #:properties [properties (hash)])
  (constructor:build-vector-action properties))


(struct build-hash-action (kind properties)
  #:transparent
  #:constructor-name constructor:build-hash-action
  #:omit-define-syntaxes)


(define (build-hash-action #:kind [kind 'equal] #:properties [properties (hash)])
  (constructor:build-hash-action kind properties))


(struct build-box-action (properties)
  #:transparent
  #:constructor-name constructor:build-box-action
  #:omit-define-syntaxes)


(define (build-box-action #:properties [properties (hash)])
  (constructor:build-box-action properties))


(struct build-prefab-struct-action (key properties)
  #:transparent
  #:constructor-name constructor:build-prefab-struct-action
  #:omit-define-syntaxes)


(define (build-prefab-struct-action key #:properties [properties (hash)])
  (constructor:build-prefab-struct-action key properties))


(define (semantic-action-build-datum-splice action children)
  (match action
    [(== cut-action) '()]
    [(== splice-action) children]
    [(? label-action?) (list (cons (label-action-value action) children))]
    [(? build-pair-action?) (list (cons (first children) (second children)))]
    [(? build-list-action?) (list children)]
    [(? build-improper-list-action?) (list (list->improper-list children))]
    [(? build-vector-action?) (list (list->vector children))]
    [(? build-hash-action?)
     (define hash-maker
       (case (build-hash-action-kind action)
         [(equal) make-immutable-hash]
         [(eqv) make-immutable-hasheqv]
         [(eq) make-immutable-hasheq]))
     (define pairs
       (for/list ([key+value (in-slice 2 children)])
         (cons (first key+value) (second key+value))))
     (define hash (hash-maker pairs))
     (unless (equal? (hash-count hash) (length pairs))
       (raise-arguments-error (name build-hash-action)
                              "duplicate keys detected when building hash datum"
                              "pairs" pairs))
     (list hash)]
    [(? build-box-action?) (list (box-immutable (first children)))]
    [(? build-prefab-struct-action?)
     (define key (build-prefab-struct-action-key action))
     (list (apply make-prefab-struct key children))]))


(define (semantic-action-build-syntax-splice action children
                                             #:first-location first-location
                                             #:last-location last-location)
  (match action
    [(== cut-action) '()]
    [(== splice-action) children]

    [(? label-action?)
     (define label-location
       (srcloc (srcloc-source first-location)
               (srcloc-line first-location)
               (srcloc-column first-location)
               (srcloc-position first-location)
               0))
     (define label-stx
       (syntax-add-properties
        (datum->syntax #false (label-action-value action) label-location #false)
        (label-action-properties action)))
     (define expression-location (srcloc-spanning first-location last-location))
     (define expression-stx
       (datum->syntax #false (cons label-stx children) expression-location #false))
     (list (syntax-add-properties expression-stx (label-action-expression-properties action)))]

    [(? build-pair-action?)
     (define location (srcloc-spanning first-location last-location))
     (define properties (build-pair-action-properties action))
     (define stx (datum->syntax #false (cons (first children) (second children)) location #false))
     (list (syntax-add-properties stx properties))]

    [(? build-list-action?)
     (define location (srcloc-spanning first-location last-location))
     (define properties (build-list-action-properties action))
     (list (syntax-add-properties (datum->syntax #false children location #false) properties))]

    [(? build-improper-list-action?)
     (define location (srcloc-spanning first-location last-location))
     (define properties (build-improper-list-action-properties action))
     (define stx (datum->syntax #false (list->improper-list children) location #false))
     (list (syntax-add-properties stx properties))]

    [(? build-vector-action?)
     (define location (srcloc-spanning first-location last-location))
     (define properties (build-vector-action-properties action))
     (define stx (datum->syntax #false (list->vector children) location #false))
     (list (syntax-add-properties stx properties))]

    [(? build-hash-action?)
     (define location (srcloc-spanning first-location last-location))
     (define hash-maker
       (case (build-hash-action-kind action)
         [(equal) make-immutable-hash]
         [(eqv) make-immutable-hasheqv]
         [(eq) make-immutable-hasheq]))
     (define pairs
       (for/list ([key+value (in-slice 2 children)])
         (cons (syntax->datum (first key+value)) (second key+value))))
     (define hash (hash-maker pairs))
     (unless (equal? (hash-count hash) (length pairs))
       (raise-arguments-error (name build-hash-action)
                              "duplicate keys detected when building hash syntax"
                              "pairs" pairs))
     (define stx (datum->syntax #false (hash-maker pairs) location #false))
     (list (syntax-add-properties stx (build-hash-action-properties action)))]

    [(? build-box-action?)
     (define location (srcloc-spanning first-location last-location))
     (define properties (build-box-action-properties action))
     (define stx (datum->syntax #false (box-immutable (first children)) location #false))
     (list (syntax-add-properties stx properties))]

    [(? build-prefab-struct-action?)
     (define location (srcloc-spanning first-location last-location))
     (define key (build-prefab-struct-action-key action))
     (define properties (build-prefab-struct-action-properties action))
     (define stx (datum->syntax #false (apply make-prefab-struct key children) location #false))
     (list (syntax-add-properties stx properties))]))


(define (list->improper-list list)
  (apply list* list))


(define (srcloc-spanning first-location last-location)
  (srcloc (srcloc-source first-location)
          (srcloc-line first-location)
          (srcloc-column first-location)
          (srcloc-position first-location)
          (- (srcloc-end-position last-location) (srcloc-position first-location))))


(define (srcloc-end-position location)
  (+ (srcloc-position location) (srcloc-span location)))


(define (syntax-add-properties stx properties)
  (for/fold ([stx stx]) ([(key value) (in-hash properties)])
    (syntax-property stx key value)))
