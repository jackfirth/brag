#lang racket/base

(require yaragg/tests/test-0n1
         yaragg/tests/test-0n1n
         yaragg/tests/test-01-equal
         yaragg/tests/test-baby-json
         yaragg/tests/test-baby-json-hider
         yaragg/tests/test-curly-quantifier
         yaragg/tests/test-cutter
         yaragg/tests/test-empty-symbol
         yaragg/tests/test-errors
         yaragg/tests/test-flatten
         yaragg/tests/test-hide-and-splice
         yaragg/tests/test-lexer
         yaragg/tests/test-nested-repeats
         yaragg/tests/test-old-token
         yaragg/tests/test-parser
         yaragg/tests/test-quotation-marks-and-backslashes
         yaragg/tests/test-simple-arithmetic-grammar
         yaragg/tests/test-simple-line-drawing
         yaragg/tests/test-start-and-atok
         yaragg/tests/test-top-level-cut
         yaragg/tests/test-weird-grammar
         yaragg/tests/test-whitespace
         yaragg/tests/test-wordy
         (submod yaragg/codegen/satisfaction test))
