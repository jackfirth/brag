#lang racket/base


(require racket/match
         rebellion/collection/vector
         rebellion/collection/vector/builder
         yaragg/lexer/private/regex-vm)


(module+ test
  (require (submod "..")
           rackunit
           rebellion/private/static-name))


;@----------------------------------------------------------------------------------------------------


(struct tagged-regular-pattern (pattern success-value) #:transparent)


(struct regular-pattern () #:transparent)


(struct char-pattern regular-pattern (expected-char) #:transparent)


(struct group-pattern regular-pattern (subpatterns capture?)
  #:transparent
  #:guard (λ (subpatterns capture? _) (values (sequence->vector subpatterns) capture?)))


(struct choice-pattern regular-pattern (choices)
  #:transparent
  #:guard (λ (choices _) (sequence->vector choices)))


(struct repetition-pattern regular-pattern (subpattern min-count max-count greedy?) #:transparent)


(struct lookahead-pattern regular-pattern (subpattern) #:transparent)


(define (regular-patterns-compile patterns)
  (define pattern-vector (sequence->vector patterns))
  (define instructions (make-vector-builder))
  (define labels (make-hash))
  (define instruction-counter 0)
  (define savepoint-counter 0)
  (define label-counter 0)

  (define (next-label!)
    (define next label-counter)
    (set! label-counter (add1 next))
    next)

  (define (next-savepoint!)
    (define next savepoint-counter)
    (set! savepoint-counter (add1 next))
    next)

  (define (label! key)
    (hash-set! labels key instruction-counter))

  (define (add-instruction! instruction)
    (vector-builder-add instructions instruction)
    (set! instruction-counter (add1 instruction-counter)))

  (define (compile-pattern! tagged-pattern)
    (match-define (tagged-regular-pattern pattern success-value) tagged-pattern)

    (let loop ([pattern pattern] [peeking? #false])
      (match pattern

        [(char-pattern expected)
         (add-instruction! (if peeking? (peek-instruction expected) (read-instruction expected)))]

        [(lookahead-pattern subpattern)
         (loop subpattern #true)
         (add-instruction! (reset-peek-instruction))]

        [(group-pattern subpatterns capture?)
         (when capture?
           (add-instruction! (save-instruction (next-savepoint!))))
         (for ([subpattern (in-vector subpatterns)])
           (loop subpattern peeking?))
         (when capture?
           (add-instruction! (save-instruction (next-savepoint!))))]

        [(choice-pattern choices)
         (define post-choice-label (next-label!))
         (for ([choice (in-vector choices 0 (- (vector-length choices) 1))])
           (define choice-label (next-label!))
           (define next-split-label (next-label!))
           (add-instruction! (labeled-split-instruction choice-label next-split-label))
           (label! choice-label)
           (loop choice peeking?)
           (add-instruction! (labeled-jump-instruction post-choice-label))
           (label! next-split-label))
         (loop (vector-ref choices (- (vector-length choices) 1)) peeking?)
         (label! post-choice-label)]

        [(repetition-pattern subpattern 0 +inf.0 greedy?)
         (define loop-label (next-label!))
         (define read-label (next-label!))
         (define skip-label (next-label!))
         (label! loop-label)
         (add-instruction!
          (if greedy?
              (labeled-split-instruction read-label skip-label)
              (labeled-split-instruction skip-label read-label)))
         (label! read-label)
         (loop subpattern peeking?)
         (add-instruction! (labeled-jump-instruction loop-label))
         (label! skip-label)]

        [(repetition-pattern subpattern 0 m greedy?)
         #:when (< m +inf.0)
         (for ([_ (in-range m)])
           (define read-label (next-label!))
           (define skip-label (next-label!))
           (add-instruction!
            (if greedy?
                (labeled-split-instruction read-label skip-label)
                (labeled-split-instruction skip-label read-label)))
           (label! read-label)
           (loop subpattern peeking?)
           (label! skip-label))]

        [(repetition-pattern subpattern n m greedy?)
         #:when (> n 0)
         (for ([_ (in-range n)])
           (loop subpattern peeking?))
         (loop (repetition-pattern subpattern 0 (- m n) greedy?) peeking?)]))

    (add-instruction! (match-instruction success-value)))

  (for ([tagged-pattern (in-vector pattern-vector 0 (sub1 (vector-length pattern-vector)))])
    (define pattern-label (next-label!))
    (define skip-label (next-label!))
    (add-instruction! (labeled-split-instruction pattern-label skip-label))
    (label! pattern-label)
    (compile-pattern! tagged-pattern)
    (label! skip-label))
  (compile-pattern! (vector-ref pattern-vector (sub1 (vector-length pattern-vector))))
  (compiled-regex-with-labels (build-vector instructions) labels))


(module+ test
  (test-case (name-string regular-patterns-compile)

    (test-case (name-string char-pattern)
      (define tagged-pattern (tagged-regular-pattern (char-pattern #\a) 42))
      (define expected (compiled-regex (list (read-instruction #\a) (match-instruction 42))))
      (check-equal? (regular-patterns-compile (list tagged-pattern)) expected))

    (test-case (name-string group-pattern)

      (test-case "non-capturing"
        (define pattern
          (group-pattern (list (char-pattern #\a) (char-pattern #\b) (char-pattern #\c)) #false))
        (define tagged-pattern (tagged-regular-pattern pattern 42))
        (define expected
          (compiled-regex
           (list
            (read-instruction #\a)
            (read-instruction #\b)
            (read-instruction #\c)
            (match-instruction 42))))
        (check-equal? (regular-patterns-compile (list tagged-pattern)) expected))

      (test-case "capturing"
        (define pattern
          (group-pattern (list (char-pattern #\a) (char-pattern #\b) (char-pattern #\c)) #true))
        (define tagged-pattern (tagged-regular-pattern pattern 42))
        (define expected
          (compiled-regex
           (list
            (save-instruction 0)
            (read-instruction #\a)
            (read-instruction #\b)
            (read-instruction #\c)
            (save-instruction 1)
            (match-instruction 42))))
        (check-equal? (regular-patterns-compile (list tagged-pattern)) expected)))

    (test-case (name-string choice-pattern)
      (define pattern
        (choice-pattern (list (char-pattern #\a) (char-pattern #\b) (char-pattern #\c))))
      (define tagged-pattern (tagged-regular-pattern pattern 42))
      (define expected
        (compiled-regex
         (list
          (split-instruction 1 3)
          (read-instruction #\a)
          (jump-instruction 7)
          (split-instruction 4 6)
          (read-instruction #\b)
          (jump-instruction 7)
          (read-instruction #\c)
          (match-instruction 42))))
      (check-equal? (regular-patterns-compile (list tagged-pattern)) expected))

    (define abc-pattern
      (group-pattern (list (char-pattern #\a) (char-pattern #\b) (char-pattern #\c)) #false))

    (test-case (name-string repetition-pattern)

      (test-case "greedy without quantifiers"
        (define pattern (repetition-pattern abc-pattern 0 +inf.0 #true))
        (define tagged-pattern (tagged-regular-pattern pattern 42))
        (define expected
          (compiled-regex
           (list
            (split-instruction 1 5)
            (read-instruction #\a)
            (read-instruction #\b)
            (read-instruction #\c)
            (jump-instruction 0)
            (match-instruction 42))))
        (check-equal? (regular-patterns-compile (list tagged-pattern)) expected))

      (test-case "non-greedy without quantifiers"
        (define pattern (repetition-pattern abc-pattern 0 +inf.0 #false))
        (define tagged-pattern (tagged-regular-pattern pattern 42))
        (define expected
          (compiled-regex
           (list
            (split-instruction 5 1)
            (read-instruction #\a)
            (read-instruction #\b)
            (read-instruction #\c)
            (jump-instruction 0)
            (match-instruction 42))))
        (check-equal? (regular-patterns-compile (list tagged-pattern)) expected))

      (test-case "greedy with minimum quantity"
        (define pattern (repetition-pattern abc-pattern 3 +inf.0 #true))
        (define tagged-pattern (tagged-regular-pattern pattern 42))
        (define expected
          (compiled-regex
           (list
            (read-instruction #\a)
            (read-instruction #\b)
            (read-instruction #\c)
            (read-instruction #\a)
            (read-instruction #\b)
            (read-instruction #\c)
            (read-instruction #\a)
            (read-instruction #\b)
            (read-instruction #\c)
            (split-instruction 10 14)
            (read-instruction #\a)
            (read-instruction #\b)
            (read-instruction #\c)
            (jump-instruction 9)
            (match-instruction 42))))
        (check-equal? (regular-patterns-compile (list tagged-pattern)) expected))

      (test-case "non-greedy with minimum quantity"
        (define pattern (repetition-pattern abc-pattern 3 +inf.0 #false))
        (define tagged-pattern (tagged-regular-pattern pattern 42))
        (define expected
          (compiled-regex
           (list
            (read-instruction #\a)
            (read-instruction #\b)
            (read-instruction #\c)
            (read-instruction #\a)
            (read-instruction #\b)
            (read-instruction #\c)
            (read-instruction #\a)
            (read-instruction #\b)
            (read-instruction #\c)
            (split-instruction 14 10)
            (read-instruction #\a)
            (read-instruction #\b)
            (read-instruction #\c)
            (jump-instruction 9)
            (match-instruction 42))))
        (check-equal? (regular-patterns-compile (list tagged-pattern)) expected))

      (test-case "greedy with maximum quantity"
        (define pattern (repetition-pattern abc-pattern 0 3 #true))
        (define tagged-pattern (tagged-regular-pattern pattern 42))
        (define expected
          (compiled-regex
           (list
            (split-instruction 1 4)
            (read-instruction #\a)
            (read-instruction #\b)
            (read-instruction #\c)
            (split-instruction 5 8)
            (read-instruction #\a)
            (read-instruction #\b)
            (read-instruction #\c)
            (split-instruction 9 12)
            (read-instruction #\a)
            (read-instruction #\b)
            (read-instruction #\c)
            (match-instruction 42))))
        (check-equal? (regular-patterns-compile (list tagged-pattern)) expected))

      (test-case "non-greedy with maximum quantity"
        (define pattern (repetition-pattern abc-pattern 0 3 #false))
        (define tagged-pattern (tagged-regular-pattern pattern 42))
        (define expected
          (compiled-regex
           (list
            (split-instruction 4 1)
            (read-instruction #\a)
            (read-instruction #\b)
            (read-instruction #\c)
            (split-instruction 8 5)
            (read-instruction #\a)
            (read-instruction #\b)
            (read-instruction #\c)
            (split-instruction 12 9)
            (read-instruction #\a)
            (read-instruction #\b)
            (read-instruction #\c)
            (match-instruction 42))))
        (check-equal? (regular-patterns-compile (list tagged-pattern)) expected))

      (test-case "greedy with minimum and maximum quantity"
        (define pattern (repetition-pattern abc-pattern 3 5 #true))
        (define tagged-pattern (tagged-regular-pattern pattern 42))
        (define expected
          (compiled-regex
           (list
            (read-instruction #\a)
            (read-instruction #\b)
            (read-instruction #\c)
            (read-instruction #\a)
            (read-instruction #\b)
            (read-instruction #\c)
            (read-instruction #\a)
            (read-instruction #\b)
            (read-instruction #\c)
            (split-instruction 10 13)
            (read-instruction #\a)
            (read-instruction #\b)
            (read-instruction #\c)
            (split-instruction 14 17)
            (read-instruction #\a)
            (read-instruction #\b)
            (read-instruction #\c)
            (match-instruction 42))))
        (check-equal? (regular-patterns-compile (list tagged-pattern)) expected))

      (test-case "non-greedy with minimum and maximum quantity"
        (define pattern (repetition-pattern abc-pattern 3 5 #false))
        (define tagged-pattern (tagged-regular-pattern pattern 42))
        (define expected
          (compiled-regex
           (list
            (read-instruction #\a)
            (read-instruction #\b)
            (read-instruction #\c)
            (read-instruction #\a)
            (read-instruction #\b)
            (read-instruction #\c)
            (read-instruction #\a)
            (read-instruction #\b)
            (read-instruction #\c)
            (split-instruction 13 10)
            (read-instruction #\a)
            (read-instruction #\b)
            (read-instruction #\c)
            (split-instruction 17 14)
            (read-instruction #\a)
            (read-instruction #\b)
            (read-instruction #\c)
            (match-instruction 42))))
        (check-equal? (regular-patterns-compile (list tagged-pattern)) expected)))

    (test-case (name-string lookahead-pattern)
      (define pattern (lookahead-pattern abc-pattern))
      (define tagged-pattern (tagged-regular-pattern pattern 42))
      (define expected
        (compiled-regex
         (list
          (peek-instruction #\a)
          (peek-instruction #\b)
          (peek-instruction #\c)
          (reset-peek-instruction)
          (match-instruction 42))))
      (check-equal? (regular-patterns-compile (list tagged-pattern)) expected))

    (test-case "multiple tagged patterns"
      (define aaa-pattern
        (group-pattern (list (char-pattern #\a) (char-pattern #\a) (char-pattern #\a)) #false))
      (define bbb-pattern
        (group-pattern (list (char-pattern #\b) (char-pattern #\b) (char-pattern #\b)) #false))
      (define ccc-pattern
        (group-pattern (list (char-pattern #\c) (char-pattern #\c) (char-pattern #\c)) #false))
      (define patterns
        (list
         (tagged-regular-pattern aaa-pattern "three As")
         (tagged-regular-pattern bbb-pattern "three Bs")
         (tagged-regular-pattern ccc-pattern "three Cs")))
      (define expected
        (compiled-regex
         (list
          (split-instruction 1 5)
          (read-instruction #\a)
          (read-instruction #\a)
          (read-instruction #\a)
          (match-instruction "three As")
          (split-instruction 6 10)
          (read-instruction #\b)
          (read-instruction #\b)
          (read-instruction #\b)
          (match-instruction "three Bs")
          (read-instruction #\c)
          (read-instruction #\c)
          (read-instruction #\c)
          (match-instruction "three Cs"))))
      (check-equal? (regular-patterns-compile patterns) expected))))
