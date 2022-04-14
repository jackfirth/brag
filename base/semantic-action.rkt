#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
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
