#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [regular-match
   (->* (exact-nonnegative-integer? exact-nonnegative-integer?)
        (#:peek-distance exact-nonnegative-integer?
         #:groups (hash/c any/c (sequence/c captured-group?)))
        regular-match?)]
  [regular-match? predicate/c]
  [regular-match-start (-> regular-match? exact-nonnegative-integer?)]
  [regular-match-end (-> regular-match? exact-nonnegative-integer?)]
  [regular-match-peek-distance (-> regular-match? exact-nonnegative-integer?)]
  [regular-match-captured-groups
   (-> regular-match? (hash/c any/c sorted-set? #:immutable #true #:flat? #true))]
  [regular-match-failure
   (-> exact-nonnegative-integer? exact-nonnegative-integer? regular-match-failure?)]
  [regular-match-failure? predicate/c]
  [regular-match-failure-start (-> regular-match-failure? exact-nonnegative-integer?)]
  [regular-match-failure-peek-distance (-> regular-match-failure? exact-nonnegative-integer?)]
  [captured-group (-> exact-nonnegative-integer? exact-nonnegative-integer? captured-group?)]
  [captured-group? predicate/c]
  [captured-group-start (-> captured-group? exact-nonnegative-integer?)]
  [captured-group-end (-> captured-group? exact-nonnegative-integer?)]
  [make-captured-groups-builder (-> captured-groups-builder?)]
  [captured-groups-builder? predicate/c]
  [captured-groups-builder-start-group!
   (-> captured-groups-builder? any/c exact-nonnegative-integer? captured-groups-builder?)]
  [captured-groups-builder-finish-group!
   (-> captured-groups-builder? any/c exact-nonnegative-integer? captured-groups-builder?)]
  [captured-groups-builder-copy (-> captured-groups-builder? captured-groups-builder?)]
  [build-captured-groups
   (-> captured-groups-builder? (hash/c any/c sorted-set? #:immutable #true #:flat? #true))]))


(require racket/match
         racket/sequence
         rebellion/base/comparator
         rebellion/collection/sorted-set
         rebellion/private/static-name
         yaragg/private/hash)


;@----------------------------------------------------------------------------------------------------


(struct regular-match (start end peek-distance captured-groups)
  #:constructor-name constructor:regular-match
  #:omit-define-syntaxes
  #:transparent

  #:guard
  (λ (position span peek-distance captured-groups _)
    (define sorted-groups
      (for/hash ([(key groups) (in-hash captured-groups)])
        (values key (sequence->sorted-set groups #:comparator captured-group<=>))))
    (values position span peek-distance sorted-groups))

  #:property prop:custom-print-quotable 'never)


(struct regular-match-failure (start peek-distance) #:transparent)


(define (regular-match start end
                       #:peek-distance [peek-distance 0]
                       #:groups [groups (hash)])
  (constructor:regular-match start end peek-distance groups))


(struct captured-group (start end) #:transparent)


(define captured-group<=>
  (comparator-chain (comparator-map natural<=> captured-group-start)
                    (comparator-map natural<=> captured-group-end)))


(struct captured-groups-builder (started-groups finished-groups))


(define (make-captured-groups-builder)
  (captured-groups-builder (make-hash) (make-hash)))


(define (captured-groups-builder-copy builder)
  (define started (hash-copy (captured-groups-builder-started-groups builder)))
  (define finished (make-hash))
  (for ([(key group-set-builder) (in-hash (captured-groups-builder-finished-groups builder))])
    (define group-set-copy
      (sorted-set-builder-add-all
       (make-sorted-set-builder captured-group<=>) (build-sorted-set group-set-builder)))
    (hash-set! finished key group-set-copy))
  (captured-groups-builder started finished))


(define (captured-groups-builder-start-group! builder key input-position)
  (define started (captured-groups-builder-started-groups builder))
  (when (hash-has-key? started key)
    (raise-arguments-error (name captured-groups-builder-start-group!)
                           "already started capturing a group for this capture key"
                           "capture key" key
                           "previous start" (hash-ref started key)
                           "next start" input-position))
  (hash-set! started key input-position)
  builder)


(define (captured-groups-builder-finish-group! builder key input-position)
  (define started (captured-groups-builder-started-groups builder))
  (unless (hash-has-key? started key)
    (raise-arguments-error (name captured-groups-builder-finish-group!)
                           "can't finish a capture group for this key, no group started yet"
                           "capture key" key
                           "finish position" input-position))
  (define finished (captured-groups-builder-finished-groups builder))
  (define start (hash-ref started key))
  (hash-remove! started key)
  (define group (captured-group start input-position))
  (define groups-for-key (hash-ref! finished key (λ () (make-sorted-set-builder captured-group<=>))))
  (sorted-set-builder-add groups-for-key group)
  builder)


(define (build-captured-groups builder)
  (define started (captured-groups-builder-started-groups builder))
  (unless (hash-empty? (captured-groups-builder-started-groups builder))
    (raise-arguments-error (name build-captured-groups)
                           "some capture groups were started but not finished"
                           "unfinished groups" started))
  (define finished (captured-groups-builder-finished-groups builder))
  (for/hash ([(key group-set-builder) (in-hash finished)])
    (values key (build-sorted-set group-set-builder))))
