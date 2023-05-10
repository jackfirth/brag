#lang racket/base


(require (for-syntax racket/base)
         racket/stxparam
         racket/vector
         rebellion/collection/vector
         syntax/parse/define
         yaragg/base/token
         yaragg/lexer/private/regular-match
         yaragg/lexer/private/regular-pattern)


(module+ test
  (require (submod "..")
           rackunit
           rebellion/private/static-name))


;@----------------------------------------------------------------------------------------------------


(struct lexer (branches delimiter-pairs)
  #:guard (λ (branches _) (sequence->vector branches))
  #:constructor-name constructor:lexer
  #:omit-define-syntaxes)


(define-syntax-parameter lexeme
  (λ (stx) (raise-syntax-error #false "must be used within lexer body" stx)))


(define-syntax-parse-rule (lexer [tag:id pattern:expr body:expr ...+] ...)
  (constructor:lexer (vector-immutable (lexer-action 'tag  pattern (lexer-handler body ...)) ...)))


(define-syntax-parse-rule (lexer-handler body ...)
  (λ (lexeme-str)
    (syntax-parameterize ([lexeme (make-rename-transformer #'lexeme-str)])
      body ...)))


(struct lexer-action (tag attributes delimiter pattern handler) #:transparent)


(struct lexer-pattern-key (index) #:transparent)


(define (lexer-pattern lexer)
  (choice-pattern
   (for/vector ([action (in-vector (lexer-branches lexer))]
                [i (in-naturals)])
     (group-pattern (list (lexer-action-pattern action)) #:capture-key (lexer-pattern-key i)))))


(define (lex-string lexer str)
  (define result (regular-pattern-match-string (lexer-pattern lexer) str))
  (cond
    [(regular-match-failure? result) #false]
    [else
     (define start (regular-match-start result))
     (define end (regular-match-end result))
     (define groups (regular-match-captured-groups result))
     (for/first ([action (in-vector (lexer-branches lexer))]
                 [i (in-naturals)]
                 #:when (hash-has-key? groups (lexer-pattern-key i)))
       (token (lexer-action-tag action) ((lexer-action-handler action) (substring str start end))))]))


(define (lexer-append . lexers)
  (constructor:lexer (apply vector-append (for/list ([l (in-list lexers)]) (lexer-branches l)))))


(define digit (element-set-pattern "0123456789"))
(define digits (repetition-pattern digit #:min-count 1))
(define sign (element-set-pattern "+-"))
(define decimal-point (element-pattern #\.))

(define unsigned-number
  (choice-pattern
   (list (group-pattern (list digits decimal-point (optional-pattern digits)))
         (group-pattern (list (optional-pattern digits) decimal-point digits))
         digits)))

(define number (group-pattern (list (optional-pattern sign) unsigned-number)))


(define number-lexer
  (lexer
   [NUMBER number (string->number lexeme)]))


(define whitespace-lexer
  (lexer
   [WHITESPACE (repetition-pattern (element-set-pattern " \n\t")) lexeme]))


(define (reserved-symbol-lexer charset)
  (constructor:lexer
   (for/vector ([char charset])
     (lexer-action (string->immutable-string (string char)) (element-pattern char) values))))


(module+ test
  (test-case (name-string number-lexer)
    (check-equal? (lex-string number-lexer "5") (token 'NUMBER 5))
    (check-equal? (lex-string number-lexer "42") (token 'NUMBER 42))
    (check-equal? (lex-string number-lexer "+5") (token 'NUMBER 5))
    (check-equal? (lex-string number-lexer "-5") (token 'NUMBER -5))
    (check-equal? (lex-string number-lexer "5.2") (token 'NUMBER 5.2))
    (check-equal? (lex-string number-lexer "5.0") (token 'NUMBER 5.0))
    (check-equal? (lex-string number-lexer "5.") (token 'NUMBER 5.0))
    (check-equal? (lex-string number-lexer ".2") (token 'NUMBER 0.2))
    (check-equal? (lex-string number-lexer "5.12345") (token 'NUMBER 5.12345))
    (check-equal? (lex-string number-lexer ".12345") (token 'NUMBER 0.12345))
    (check-equal? (lex-string number-lexer "9876.12345") (token 'NUMBER 9876.12345))
    (check-equal? (lex-string number-lexer "+5.2") (token 'NUMBER 5.2))
    (check-equal? (lex-string number-lexer "+5.0") (token 'NUMBER 5.0))
    (check-equal? (lex-string number-lexer "+5.") (token 'NUMBER 5.0))
    (check-equal? (lex-string number-lexer "+.2") (token 'NUMBER 0.2))
    (check-equal? (lex-string number-lexer "+5.12345") (token 'NUMBER 5.12345))
    (check-equal? (lex-string number-lexer "+.12345") (token 'NUMBER 0.12345))
    (check-equal? (lex-string number-lexer "+9876.12345") (token 'NUMBER 9876.12345))
    (check-equal? (lex-string number-lexer "-5.2") (token 'NUMBER -5.2))
    (check-equal? (lex-string number-lexer "-5.0") (token 'NUMBER -5.0))
    (check-equal? (lex-string number-lexer "-5.") (token 'NUMBER -5.0))
    (check-equal? (lex-string number-lexer "-.2") (token 'NUMBER -0.2))
    (check-equal? (lex-string number-lexer "-5.12345") (token 'NUMBER -5.12345))
    (check-equal? (lex-string number-lexer "-.12345") (token 'NUMBER -0.12345))
    (check-equal? (lex-string number-lexer "-9876.12345") (token 'NUMBER -9876.12345))))
