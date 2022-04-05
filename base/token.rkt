#lang racket/base


(require (for-syntax racket/base
                     syntax/parse)
         racket/contract/base
         racket/match
         racket/symbol
         rebellion/private/printer-markup)


(provide
 (struct-out token)
 (contract-out
  [syntax-token
   (->* (any/c #:position exact-positive-integer? #:span exact-nonnegative-integer?)
        (any/c
         #:source any/c
         #:line (or/c exact-positive-integer? #false)
         #:column (or/c exact-nonnegative-integer? #false)
         #:skip? boolean?
         #:properties hash?)
        syntax-token?)]
  [syntax-token? predicate/c]
  [syntax-token-type (-> syntax-token? any/c)]
  [syntax-token-value (-> syntax-token? any/c)]
  [syntax-token-location (-> syntax-token? srcloc?)]
  [syntax-token-skip? (-> syntax-token? boolean?)]
  [syntax-token-source (-> syntax-token? any/c)]
  [syntax-token-position (-> syntax-token? exact-positive-integer?)]
  [syntax-token-span (-> syntax-token? exact-nonnegative-integer?)]
  [syntax-token-end-position (-> syntax-token? exact-positive-integer?)]
  [syntax-token-line (-> syntax-token? (or/c exact-positive-integer? #false))]
  [syntax-token-column (-> syntax-token? (or/c exact-nonnegative-integer? #false))]
  [syntax-token-properties (-> syntax-token? hash?)]
  [syntax-token->syntax (-> syntax-token? syntax?)]))


;@----------------------------------------------------------------------------------------------------


(struct token (type value) #:transparent)


(struct syntax-token (type value source position span line column skip? properties)
  #:constructor-name constructor:token
  #:transparent
  #:omit-define-syntaxes

  #:property prop:custom-print-quotable 'never

  #:methods gen:custom-write

  [(define write-proc
     (make-constructor-style-printer-with-markup
      'token
      (λ (this)
        (append (list (syntax-token-type this))
                (if (syntax-token-value this) (list (syntax-token-value this)) '())
                (optional-keyword-argument-markup "#:source" (syntax-token-source this))
                (optional-keyword-argument-markup "#:position" (syntax-token-position this))
                (optional-keyword-argument-markup "#:span" (syntax-token-span this))
                (optional-keyword-argument-markup "#:line" (syntax-token-line this))
                (optional-keyword-argument-markup "#:column" (syntax-token-column this))
                (optional-keyword-argument-markup "#:skip?" (syntax-token-skip? this))
                (optional-keyword-argument-markup "#:properties" (syntax-token-properties this))))))])


(define (syntax-token type
                      [value type]
                      #:source [source #false]
                      #:position position
                      #:span span
                      #:line [line #false]
                      #:column [column #false]
                      #:skip? [skip? #false]
                      #:properties [properties (hash)])
  (constructor:token type value source position span line column skip? properties))


(define (syntax-token-location token)
  (srcloc (syntax-token-source token)
          (syntax-token-line token)
          (syntax-token-column token)
          (syntax-token-position token)
          (syntax-token-span token)))


(define (syntax-token->syntax token)
  (for/fold ([stx (datum->syntax #false (syntax-token-value token) (syntax-token-location token))])
            ([(key value) (in-hash (syntax-token-properties token))])
    (syntax-property stx key value)))


(define (optional-keyword-argument-markup kw-string value)
  (if value (list (sequence-markup (list (unquoted-printing-string kw-string) value))) '()))


(define (syntax-token-end-position token)
  (+ (syntax-token-position token) (syntax-token-span token)))


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
