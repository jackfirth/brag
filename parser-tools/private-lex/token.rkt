#lang racket/base
(require (for-syntax racket/base yaragg/parser-tools/private-lex/token-syntax))
  
;; Defining tokens
  
(provide define-tokens define-empty-tokens token token?
         (protect-out (rename-out [token-name real-token-name]))
         (protect-out (rename-out [token-value real-token-value]))
         (rename-out [token-name* token-name][token-value* token-value])
         (struct-out position)
         (struct-out position-token)
         (struct-out srcloc-token))

  
;; A token is either
;; - symbol
;; - (token symbol any)
(struct token (name value) #:transparent)

;; token-name*: token -> symbol
(define (token-name* t)
  (cond
    [(symbol? t) t]
    [(token? t) (token-name t)]
    [else (raise-type-error 'token-name "symbol or struct:token" 0 t)]))
  
;; token-value*: token -> any
(define (token-value* t)
  (cond
    [(symbol? t) #f]
    [(token? t) (token-value t)]
    [else (raise-type-error 'token-value "symbol or struct:token" 0 t)]))
  
(define-for-syntax (make-ctor-name n)
  (datum->syntax n
                 (string->symbol (format "token-~a" (syntax-e n)))
                 n
                 n))
  
(define-for-syntax ((make-define-tokens empty?) stx)
  (syntax-case stx ()
    [(_ NAME (TOKEN ...))
     (andmap identifier? (syntax->list #'(TOKEN ...)))
     (with-syntax (((marked-token ...)
                    (map values #;(make-syntax-introducer)
                         (syntax->list #'(TOKEN ...)))))
       (quasisyntax/loc stx
         (begin
           (define-syntax NAME
             #,(if empty?
                   #'(e-terminals-def (quote-syntax (marked-token ...)))
                   #'(terminals-def (quote-syntax (marked-token ...)))))
           #,@(map
               (λ (n)
                 (when (eq? (syntax-e n) 'error)
                   (raise-syntax-error
                    #f
                    "Cannot define a token named error."
                    stx))
                 (if empty?
                     #`(define (#,(make-ctor-name n))
                         '#,n)
                     #`(define (#,(make-ctor-name n) x)
                         (token '#,n x))))
               (syntax->list #'(TOKEN ...)))
           #;(define marked-token #f) #;...)))]
    [(_ ...)
     (raise-syntax-error #f
      "must have the form (define-tokens name (identifier ...)) or (define-empty-tokens name (identifier ...))"
      stx)]))
  
(define-syntax define-tokens (make-define-tokens #f))
(define-syntax define-empty-tokens (make-define-tokens #t))

(struct position (offset line col) #:inspector #f)
(struct position-token (token start-pos end-pos) #:inspector #f)

(struct srcloc-token (token srcloc) #:inspector #f)
  

