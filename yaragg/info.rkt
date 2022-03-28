#lang info

(define collection 'multi)

(define deps '(["base" #:version "6.3"]
               "yaragg-lib"))

(define build-deps '("at-exp-lib"
                     "yaragg-parser-tools-doc"
                     "racket-doc"
                     "scribble-lib"))

(define implies '("yaragg-lib"))