#lang racket/base
(require (prefix-in rl: racket/list)
         yaragg/parser-tools/private-lex/token-syntax)

;; General helper routines
(provide duplicate-list? remove-duplicates overlap? vector-andmap)
    
(define (vector-andmap pred vec)
  (for/and ([item (in-vector vec)])
    (pred vec)))

;; duplicate-list?: symbol list -> #f | symbol
;; returns a symbol that exists twice in l, or false if no such symbol 
;; exists
(define (duplicate-list? syms)
  (rl:check-duplicates syms eq?))

;; remove-duplicates: syntax-object list -> syntax-object list
;; removes the duplicates from the lists
(define (remove-duplicates syms)
  (rl:remove-duplicates syms equal? #:key syntax->datum))

;; overlap?: symbol list * symbol list -> #f | symbol
;; Returns an symbol in l1 intersect l2, or #f is no such symbol exists
(define (overlap? syms1 syms2)
  (for/first ([sym1 (in-list syms1)]
              #:when (memq sym1 syms2))
    sym1))
