#lang racket/base

(module+ test

  (require yaragg/examples/wordy
           yaragg/support
           rackunit)

  (check-equal?
   (syntax->datum
    (parse (list "hello" "world")))
   '(sentence (verb (greeting "hello")) (optional-adjective) (object "world")))



  (check-equal?
   (syntax->datum
    (parse (list "hola" "frumpy" (token 'WORLD "세계"))))
 
   '(sentence (verb (greeting "hola")) (optional-adjective "frumpy") (object "세계"))))
