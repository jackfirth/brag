#lang info

(define collection 'multi)
(define deps '("base"))
(define build-deps '("scheme-lib"
                     "racket-doc"
                     "syntax-color-doc"
                     "yaragg-parser-tools-lib"
                     "scribble-lib"))
(define update-implies '("yaragg-parser-tools-lib"))
