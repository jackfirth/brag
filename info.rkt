#lang info


(define collection "yaragg")


(define scribblings
  '(("yaragg.scrbl" (multi-page) (parsing-library))))


(define deps '(["base" #:version "6.3"]
               "rebellion"
               "syntax-color-lib"))


(define build-deps '("at-exp-lib"
                     "racket-doc"
                     "rackunit-lib"
                     "scribble-lib"
                     "syntax-color-doc"))


(define test-omit-paths '("examples/simple-line-drawing/examples/letter-i.rkt"))
