#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [hash->immutable-hash (-> hash? (and/c hash? immutable?))]))


;@----------------------------------------------------------------------------------------------------


(define hash->immutable-hash
  (lambda (h)
    (if (and (hash? h) (immutable? h))
        h
        (for/hash ([(k v) (in-hash h)])
          (values k v)))))
