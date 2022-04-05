#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [parser? predicate/c]
  [parse-datum (-> parser? (sequence/c token?) parser-derivation?)]
  [parse-syntax (-> parser? (sequence/c syntax-token?) syntax?)]
  [parse-ambiguous-datum (-> parser? (sequence/c token?) (set/c parser-derivation?))]
  [parse-ambiguous-syntax (-> parser? (sequence/c syntax-token?) (set/c syntax?))]))


(module+ private
  (provide
   (contract-out
    [make-parser
     (-> #:datum-function (-> (sequence/c token?) (stream/c parser-derivation?))
         #:syntax-function (-> (sequence/c syntax-token?) (stream/c syntax?))
         parser?)])))


(require racket/sequence
         racket/set
         racket/stream
         yaragg/base/token
         yaragg/base/derivation)


;@----------------------------------------------------------------------------------------------------


(struct parser (datum-function syntax-function))


(define (make-parser #:datum-function datum-function #:syntax-function syntax-function)
  (parser datum-function syntax-function))


(define (parse-ambiguous-syntax p tokens)
  (for/set ([stx (in-stream ((parser-syntax-function p) tokens))])
    stx))


(define (parse-ambiguous-datum p tokens)
  (for/set ([derivation (in-stream ((parser-datum-function p) tokens))])
    derivation))


(define (parse-syntax p tokens)
  (define stx-stream ((parser-syntax-function p) tokens))
  (when (stream-empty? stx-stream)
    (raise-arguments-error 'parse-syntax "no parse trees produced" "parser" p "tokens" tokens))
  (define stx (stream-first stx-stream))
  (unless (stream-empty? (stream-rest stx-stream))
    (raise-arguments-error 'parse-syntax
                           "ambiguous parse, multiple parse trees produced"
                           "parser" p
                           "tokens" tokens
                           "first parse tree" stx
                           "second parse tree" (stream-first (stream-rest stx-stream))))
  stx)


(define (parse-datum p tokens)
  (define derivation-stream ((parser-datum-function p) tokens))
  (when (stream-empty? derivation-stream)
    (raise-arguments-error 'parse-datum "no parse trees produced" "parser" p "tokens" tokens))
  (define derivation (stream-first derivation-stream))
  (unless (stream-empty? (stream-rest derivation-stream))
    (raise-arguments-error 'parse-datum
                           "ambiguous parse, multiple parse trees produced"
                           "parser" p
                           "tokens" tokens
                           "first parse tree" derivation
                           "second parse tree" (stream-first (stream-rest derivation-stream))))
  derivation)
