#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [parser? predicate/c]
  [parse-datum (-> parser? (sequence/c token?) any/c)]
  [parse-syntax (-> parser? (sequence/c syntax-token?) syntax?)]
  [parse-ambiguous-datum (-> parser? (sequence/c token?) (set/c any/c))]
  [parse-ambiguous-syntax (-> parser? (sequence/c syntax-token?) (set/c syntax?))]))


(module+ private
  (provide
   (contract-out
    [make-parser
     (-> #:deriver (-> (sequence/c token?) (stream/c parser-derivation?)) parser?)])))


(require racket/sequence
         racket/set
         racket/stream
         yaragg/base/token
         yaragg/base/derivation)


;@----------------------------------------------------------------------------------------------------


(struct parser (deriver))


(define (make-parser #:deriver deriver)
  (parser deriver))


(define (parse-ambiguous-syntax p tokens)
  (for/set ([derivation (in-stream ((parser-deriver p) tokens))])
    (parser-derivation->syntax derivation)))


(define (parse-ambiguous-datum p tokens)
  (for/set ([derivation (in-stream ((parser-deriver p) tokens))])
    (parser-derivation->datum derivation)))


(define (parse-syntax p token-sequence)
  (define tokens
    (for/vector ([t token-sequence])
      (token (syntax-token-type t) t)))
  (define derivations ((parser-deriver p) tokens))
  (when (stream-empty? derivations)
    (raise-arguments-error 'parse-syntax "no parse trees produced" "parser" p "tokens" tokens))
  (define stx (parser-derivation->syntax (stream-first derivations)))
  (unless (stream-empty? (stream-rest derivations))
    (raise-arguments-error 'parse-syntax
                           "ambiguous parse, multiple parse trees produced"
                           "parser" p
                           "tokens" tokens
                           "first parse tree" stx
                           "second parse tree"
                           (parser-derivation->syntax (stream-first (stream-rest derivations)))))
  stx)


(define (parse-datum p tokens)
  (define derivations ((parser-deriver p) tokens))
  (when (stream-empty? derivations)
    (raise-arguments-error 'parse-datum "no parse trees produced" "parser" p "tokens" tokens))
  (define datum (parser-derivation->datum (stream-first derivations)))
  (unless (stream-empty? (stream-rest derivations))
    (raise-arguments-error 'parse-datum
                           "ambiguous parse, multiple parse trees produced"
                           "parser" p
                           "tokens" tokens
                           "first parse tree" datum
                           "second parse tree"
                           (parser-derivation->datum (stream-first (stream-rest derivations)))))
  datum)
