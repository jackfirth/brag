#lang racket/base
(require yaragg/parser-tools/private-yacc/grammar)
(provide (except-out (all-defined-out) reduce reduce*)
         (rename-out [reduce* reduce]))

;; An action is 
;;  - (shift int)
;;  - (reduce prod runtime-action)
;;  - (accept)
;;  - (goto int)
;;  - (no-action)
;; A reduce contains a runtime-reduce so that sharing of the reduces can
;; be easily transferred to sharing of runtime-reduces.
  
(struct action () #:transparent)
(struct shift action (state) #:transparent)
(struct reduce action (prod runtime-reduce) #:transparent)
(struct accept action () #:transparent)
(struct goto action (state) #:transparent)
(struct no-action action () #:transparent)
  
(define (reduce* p)
  (reduce p
          (vector (prod-index p)
                  (gram-sym-symbol (prod-lhs p))
                  (vector-length (prod-rhs p)))))

;; A runtime-action is
;; non-negative-int        (shift)
;; (vector int symbol int) (reduce)
;; 'accept                 (accept)
;; negative-int            (goto)
;; #f                      (no-action)
  
(define (action->runtime-action a)
  (cond
    [(shift? a) (shift-state a)]
    [(reduce? a) (reduce-runtime-reduce a)]
    [(accept? a) 'accept]
    [(goto? a) (- (+ (goto-state a) 1))]
    [(no-action? a) #f]))
  
(define (runtime-shift? x) (and (integer? x) (>= x 0)))
(define runtime-reduce? vector?)
(define (runtime-accept? x) (eq? x 'accept))
(define (runtime-goto? x) (and (integer? x) (< x 0)))

(define runtime-shift-state values) 
(define (runtime-reduce-prod-num x) (vector-ref x 0))
(define (runtime-reduce-lhs x) (vector-ref x 1))
(define (runtime-reduce-rhs-length x) (vector-ref x 2))
(define (runtime-goto-state x) (- (+ x 1)))


