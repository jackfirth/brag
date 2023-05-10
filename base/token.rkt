#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [token? (-> any/c boolean?)]
  [atom? (-> any/c boolean?)]
  [atom (->* (symbol? any/c) (#:properties hash?) atom?)]
  [atom-type (-> atom? symbol?)]
  [atom-datum (-> atom? any/c)]
  [atom-properties (-> atom? hash?)]
  [punctuation? (-> any/c boolean?)]
  [punctuation (-> string? punctuation?)]
  [punctuation-string (-> punctuation? (and/c string? immutable?))]
  [comment? (-> any/c boolean?)]
  [comment (-> string? comment?)]
  [comment-text (-> comment? (and/c string? immutable?))]
  [whitespace? (-> any/c boolean?)]
  [whitespace
   (->* () (#:linebreak-count exact-nonnegative-integer? #:space-count exact-nonnegative-integer?)
        whitespace?)]
  [whitespace-linebreak-count (-> whitespace? exact-nonnegative-integer?)]
  [whitespace-space-count (-> whitespace? exact-nonnegative-integer?)]
  [lexeme? (-> any/c boolean?)]
  [lexeme (-> token? srcloc? lexeme?)]
  [lexeme-token (-> lexeme? token?)]
  [lexeme-location (-> lexeme? srcloc?)]
  [atom-lexeme? (-> any/c boolean?)]
  [atom-lexeme->syntax (-> lexeme? syntax?)]))


(require racket/match)


;@----------------------------------------------------------------------------------------------------


(struct token () #:transparent)


(struct atom token (type datum properties)
  #:transparent
  #:name type:atom
  #:constructor-name constructor:atom)


(define (atom type datum #:properties [properties (hash)])
  (let ([properties
         (if (immutable? properties)
             properties
             (for/hash ([(k v) (in-hash properties)])
               (values k v)))])
    (constructor:atom type datum properties)))


(struct punctuation token (string) #:transparent)


(struct comment token (text) #:transparent)


(struct whitespace token (linebreak-count space-count)
  #:transparent
  #:omit-define-syntaxes
  #:constructor-name constructor:whitespace)


(define (whitespace #:linebreak-count [linebreaks 0] #:space-count [spaces 0])
  (constructor:whitespace linebreaks spaces))


(struct lexeme (token location) #:transparent)


(define (atom-lexeme? v)
  (and (lexeme? v) (atom? (lexeme-token v))))


(define (atom-lexeme->syntax l)
  (match-define (lexeme (type:atom _ datum properties) location) l)
  (for/fold ([stx (datum->syntax #false datum location #false)])
            ([(k v) (in-hash properties)])
    (syntax-property stx k v)))
