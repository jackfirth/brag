#lang racket/base
(require (for-syntax racket/base)
         yaragg-parser-tools/lex
         (prefix-in : yaragg-parser-tools/lex-sre))
  
(provide epsilon ~
         (rename-out [:* *]
                     [:+ +]
                     [:? ?]
                     [:or :]
                     [:& &]
                     [:: @]
                     [:~ ^]
                     [:/ -]))
           
(define-lex-trans (epsilon stx)
  (syntax-case stx ()
    [(_) #'""]))
  
(define-lex-trans (~ stx)
  (syntax-case stx ()
    [(_ RE) #'(complement RE)]))  
  
