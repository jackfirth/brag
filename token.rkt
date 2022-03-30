#lang racket/base


(require (for-syntax racket/base
                     syntax/parse)
         racket/contract/base
         racket/match
         racket/symbol
         rebellion/private/printer-markup)


(provide
 token
 (contract-out
  [token? predicate/c]
  [token-type (-> token? (and/c symbol? symbol-interned?))]
  [token-value (-> token? any/c)]
  [token-location (-> token? (or/c srcloc? #false))]
  [token-skip? (-> token? boolean?)]
  [source-location
   (->* ()
        (#:source any/c
         #:position (or/c exact-positive-integer? #false)
         #:line (or/c exact-positive-integer? #false)
         #:column (or/c exact-nonnegative-integer? #false)
         #:span (or/c exact-nonnegative-integer? #false))
        srcloc?)]
  [token-position (-> token? (or/c exact-positive-integer? #false))]
  [token-end-position (-> token? (or/c exact-positive-integer? #false))]))


;@----------------------------------------------------------------------------------------------------


(struct token (type value location skip?)
  #:constructor-name constructor:token
  #:transparent
  #:omit-define-syntaxes

  #:property prop:custom-print-quotable 'never

  #:methods gen:custom-write

  [(define write-proc
     (make-constructor-style-printer-with-markup
      'token
      (λ (this)
        (append (list (token-type this))
                (if (token-value this) (list (token-value this)) '())
                (if (token-location this)
                    (list
                     (sequence-markup
                      (list (unquoted-printing-string "#:location") (token-location this))))
                    '())
                (if (token-skip? this)
                    (list
                     (sequence-markup (list (unquoted-printing-string "#:skip?") (token-skip? this))))
                    '())))))])


(define (source-location #:source [source #false]
                         #:position [position #false]
                         #:span [span #false]
                         #:line [line #false]
                         #:column [column #false])
  (srcloc source line column position span))


(define (unchecked:token type
                         [value #false]
                         #:location [location #false]
                         #:skip? [skip? #false])
  (let ([type
         (cond [(string? type) (string->symbol type)]
               [(not (symbol-interned? type)) (string->symbol (symbol->immutable-string type))]
               [else type])])
    (constructor:token type value location skip?)))


(define-module-boundary-contract contracted:token unchecked:token
  (->* ((or/c symbol? string?)) (any/c #:location (or/c srcloc? #false) #:skip? boolean?)
       token?)
  #:name-for-blame token)


(define-match-expander token
  (syntax-parser
    [(_
      (~alt
       (~once value-pattern:expr)
       (~optional (~seq #:type type-pattern:expr) #:defaults ([type-pattern #'_]))
       (~optional (~seq #:location location-pattern:expr) #:defaults ([location-pattern #'_]))
       (~optional (~seq #:skip? skip-pattern:expr) #:defaults ([skip-pattern #'_])))
      ...)
     #'(? token?
          (app token-value value-pattern)
          (app token-type type-pattern)
          (app token-location location-pattern)
          (app token-skip? skip-pattern))])
  (make-rename-transformer #'contracted:token))


(define (token-position t)
  (define loc (token-location t))
  (and loc (srcloc-position loc)))


(define (token-end-position t)
  (define loc (token-location t))
  (define start (and loc (srcloc-position loc)))
  (define span (and loc (srcloc-span loc)))
  (and start span (+ start span)))


#|
;; yaragg/support
(struct token-struct (type val offset line column span skip?)
  #:auto-value #f
  #:transparent)

;; yaragg/parser-tools/cfg-parser
(struct tok (name orig-name val start end))

;; yaragg/parser-tools/private-lex/token
(struct token (name value) #:transparent)
(struct position-token (token start-pos end-pos) #:inspector #f)
(struct srcloc-token (token srcloc) #:inspector #f)
|#
