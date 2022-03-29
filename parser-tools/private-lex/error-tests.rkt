#lang racket/base

(module+ test

  (require (for-syntax racket/base) ; keep: convert-compile-time-error is hiding usages
           yaragg/parser-tools/lex ; keep: convert-compile-time-error is hiding usages
           rackunit
           syntax/macro-testing)

  (check-exn #rx"lex-abbrev" (λ () (convert-compile-time-error (define-lex-abbrev))))
  (check-exn #rx"lex-abbrev" (λ () (convert-compile-time-error (define-lex-abbrev a))))
  (check-exn #rx"lex-abbrev" (λ () (convert-compile-time-error (define-lex-abbrev (a b) v))))
  (check-exn #rx"lex-abbrev" (λ () (convert-compile-time-error (define-lex-abbrev 1 1))))
  (check-exn #rx"lex-abbrevs" (λ () (convert-compile-time-error (define-lex-abbrevs ()))))

  (check-exn #rx"lex-trans" (λ () (convert-compile-time-error (define-lex-trans))))

  (check-exn #rx"lexer" (λ () (convert-compile-time-error (lexer))))
  (check-exn #rx"lexer" (λ () (convert-compile-time-error (lexer ("a" "b" "c")))))
  (check-exn #rx"lexer" (λ () (convert-compile-time-error (lexer ()))))
  (check-exn #rx"lexer" (λ () (convert-compile-time-error (lexer ("")))))

  (check-exn #rx"regular-expression" (λ () (convert-compile-time-error (lexer (a 1)))))
  (check-exn #rx"regular-expression" (λ () (convert-compile-time-error (lexer ((a) 1)))))
  (check-exn #rx"regular-expression"
             (λ () (convert-compile-time-error (let ((a 1)) (lexer ((a) 1))))))

  (check-exn #rx"regular-expression"
             (λ () (convert-compile-time-error (let-syntax ((a 1)) (lexer ((a) 1))))))

  (check-exn #rx"define-lex-trans"
             (λ ()
               (convert-compile-time-error 
                (let ()
                  (define-lex-trans a 1)
                  (let ()
                    (lexer ((a) 1)))))))

  ;; Detecting mutual recursion cycle:
  (check-exn #rx"regular-expression"
             (λ ()
               (convert-compile-time-error 
                (let ()
                  (define-lex-abbrev a b)
                  (define-lex-abbrev b a)
                  (let ()
                    (lexer (a 1)))))))

  (check-exn #rx"regular-expression"
             (λ ()
               (convert-compile-time-error 
                (let ()
                  (define-lex-abbrev a (repetition 0 1 b))
                  (define-lex-abbrev b (repetition 0 1 a))
                  (let ()
                    (lexer (a 1)))))))

  ;; Detecting cycle within same abbreviation:
  (check-exn #rx"regular-expression"
             (λ ()
               (convert-compile-time-error 
                (let ()
                  (define-lex-abbrev balanced 
                    (union (concatenation "(" balanced ")" balanced)
                           any-char))
                  (lexer
                   [balanced (string-append lexeme (balanced input-port))]
                   [(eof) ""])))))


  (check-exn #rx"regular-expression" (λ () (convert-compile-time-error (lexer (1 1)))))
  (check-exn #rx"repetition" (λ () (convert-compile-time-error (lexer ((repetition) 1)))))
  (check-exn #rx"repetition" (λ () (convert-compile-time-error (lexer ((repetition #\1 #\1 "3") 1)))))
  (check-exn #rx"repetition" (λ () (convert-compile-time-error (lexer ((repetition 1 #\1 "3") 1)))))
  (check-exn #rx"repetition" (λ () (convert-compile-time-error (lexer ((repetition 1 0 "3") 1)))))
  (check-exn #rx"complement" (λ () (convert-compile-time-error (lexer ((complement) 1)))))
  (check-exn #rx"char-range" (λ () (convert-compile-time-error (lexer ((char-range) 1)))))
  (check-exn #rx"char-range" (λ () (convert-compile-time-error (lexer ((char-range #\9 #\0) 1)))))
  (check-exn #rx"char-complement" (λ () (convert-compile-time-error (lexer ((char-complement) 1)))))
  (check-exn #rx"char-complement"
             (λ ()
               (convert-compile-time-error (lexer ((char-complement (concatenation "1" "2")) 1))))))
