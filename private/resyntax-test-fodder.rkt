#lang racket/base


(define (foo x)
  (println "before bar definition")
  (define (bar)
    (+ 1 2 3))
  (println "after bar definition")
  (bar))
