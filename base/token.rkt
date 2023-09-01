#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [token? predicate/c]
  [token-location (-> token? srcloc?)]
  [atom? predicate/c]
  [atom
   (->* (symbol? any/c)
        (#:properties hash?
         #:location (or/c srcloc? #false)
         #:source any/c
         #:position (or/c exact-positive-integer? #false)
         #:span (or/c exact-nonnegative-integer? #false)
         #:line (or/c exact-positive-integer? #false)
         #:column (or/c exact-nonnegative-integer? #false))
        atom?)]
  [atom-type (-> atom? symbol?)]
  [atom-datum (-> atom? any/c)]
  [atom-properties (-> atom? hash?)]
  [atom->syntax (-> atom? syntax?)]
  [punctuation? predicate/c]
  [punctuation
   (->* (string?)
        (#:location (or/c srcloc? #false)
         #:source any/c
         #:position (or/c exact-positive-integer? #false)
         #:span (or/c exact-nonnegative-integer? #false)
         #:line (or/c exact-positive-integer? #false)
         #:column (or/c exact-nonnegative-integer? #false))
        punctuation?)]
  [punctuation-string (-> punctuation? (and/c string? immutable?))]
  [comment? predicate/c]
  [comment
   (->* (string?)
        (#:location (or/c srcloc? #false)
         #:source any/c
         #:position (or/c exact-positive-integer? #false)
         #:span (or/c exact-nonnegative-integer? #false)
         #:line (or/c exact-positive-integer? #false)
         #:column (or/c exact-nonnegative-integer? #false))
        comment?)]
  [comment-text (-> comment? (and/c string? immutable?))]
  [whitespace? predicate/c]
  [whitespace
   (->* ()
        (#:linebreak-count exact-nonnegative-integer?
         #:space-count exact-nonnegative-integer?
         #:location (or/c srcloc? #false)
         #:source any/c
         #:position (or/c exact-positive-integer? #false)
         #:span (or/c exact-nonnegative-integer? #false)
         #:line (or/c exact-positive-integer? #false)
         #:column (or/c exact-nonnegative-integer? #false))
        whitespace?)]
  [whitespace-linebreak-count (-> whitespace? exact-nonnegative-integer?)]
  [whitespace-space-count (-> whitespace? exact-nonnegative-integer?)]))


(require racket/match)


;@----------------------------------------------------------------------------------------------------


(struct token (location) #:transparent)


(struct atom token (type datum properties)
  #:transparent
  #:name type:atom
  #:constructor-name constructor:atom)


(define (atom type datum
              #:properties [properties (hash)]
              #:location [location #false]
              #:source [source #false]
              #:position [position #false]
              #:span [span #false]
              #:line [line #false]
              #:column [column #false])
  (define properties*
    (if (immutable? properties)
        properties
        (for/hash ([(k v) (in-hash properties)])
          (values k v))))
  (define loc (or location (srcloc source line column position span)))
  (constructor:atom loc type datum properties))


(struct punctuation token (string)
  #:transparent
  #:omit-define-syntaxes
  #:constructor-name constructor:punctuation)


(define (punctuation string
                     #:location [location #false]
                     #:source [source #false]
                     #:position [position #false]
                     #:span [span #false]
                     #:line [line #false]
                     #:column [column #false])
  (define span* (or span (and position (+ position (string-length string)))))
  (define loc (or location (srcloc source line column position span*)))
  (constructor:punctuation loc (string->immutable-string string)))


(struct comment token (text)
  #:transparent
  #:omit-define-syntaxes
  #:constructor-name constructor:comment)


(define (comment text
                 #:location [location #false]
                 #:source [source #false]
                 #:position [position #false]
                 #:span [span #false]
                 #:line [line #false]
                 #:column [column #false])
  (define span* (or span (and position (+ position (string-length text)))))
  (define loc (or location (srcloc source line column position span*)))
  (constructor:punctuation loc (string->immutable-string string)))


(struct whitespace token (linebreak-count space-count)
  #:transparent
  #:omit-define-syntaxes
  #:constructor-name constructor:whitespace)


(define (whitespace #:linebreak-count [linebreaks 0]
                    #:space-count [spaces 0]
                    #:location [location #false]
                 #:source [source #false]
                 #:position [position #false]
                 #:span [span #false]
                 #:line [line #false]
                 #:column [column #false])
  (define span* (or span (and position (+ position linebreaks spaces))))
  (define loc (or location (srcloc source line column position span*)))
  (constructor:whitespace loc linebreaks spaces))


(define (atom->syntax l)
  (match-define (type:atom location _ datum properties) l)
  (for/fold ([stx (datum->syntax #false datum location #false)])
            ([(k v) (in-hash properties)])
    (syntax-property stx k v)))
