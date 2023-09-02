#lang racket/base


(define foo
  (lambda (x)
    (println "before bar definition")
    (define (bar)
      (+ 1 2 3 4 5 6))
    (println "after bar definition")
    (bar)))


(define baz
  (lambda (x y z)
    42))
