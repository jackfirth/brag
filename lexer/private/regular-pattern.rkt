#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [regular-pattern? predicate/c]
  [element-pattern (-> any/c regular-pattern?)]
  [element-string-pattern (-> (sequence/c any/c) regular-pattern?)]
  [element-set-pattern (-> (sequence/c any/c) regular-pattern?)]
  [group-pattern
   (->* ((sequence/c regular-pattern?)) (#:capture-key (not/c #false)) regular-pattern?)]
  [choice-pattern (-> (sequence/c regular-pattern?) regular-pattern?)]
  [repetition-pattern
   (->i ([subpattern regular-pattern?])
        (#:min-count [min-count exact-nonnegative-integer?]
         #:max-count [max-count (or/c exact-nonnegative-integer? +inf.0)]
         #:greedy? [_ boolean?])
        #:pre/name (min-count max-count)
        "minimum repetition count cannot be greater than the maximum repetition count"
        (or (unsupplied-arg? min-count) (unsupplied-arg? max-count) (<= min-count max-count))
        [_ regular-pattern?])]
  [optional-pattern (->* (regular-pattern?) (#:greedy? boolean?) regular-pattern?)]
  [lookahead-pattern (-> regular-pattern? regular-pattern?)]
  [regular-pattern-compile (-> regular-pattern? compiled-regex?)]))


(require racket/match
         racket/sequence
         racket/set
         rebellion/base/option
         rebellion/collection/vector
         rebellion/collection/vector/builder
         rebellion/streaming/transducer
         yaragg/lexer/private/regex-vm)


(module+ test
  (require (submod "..")
           rackunit
           rebellion/private/static-name))


;@----------------------------------------------------------------------------------------------------


(struct regular-pattern () #:transparent)


(struct element-pattern regular-pattern (expected-char) #:transparent)


(define (element-string-pattern elements)
  (group-pattern (for/vector ([e elements]) (element-pattern e))))


(define (element-set-pattern elements)
  ;; We deduplicate and build a vector instead of building a set to ensure the order of the elements
  ;; is preserved. This isn't strictly necessary, but it makes the API and tests deterministic.
  (define choices
    (transduce elements
               (deduplicating)
               (mapping element-pattern)
               #:into (into-vector)))
  (choice-pattern choices))


(struct group-pattern regular-pattern (subpatterns capture-key)
  #:transparent
  #:name struct-transformer:group-pattern
  #:constructor-name constructor:group-pattern
  #:guard (λ (subpatterns capture-key _) (values (sequence->vector subpatterns) capture-key)))


(define (group-pattern #:capture-key [capture-key #false] subpatterns)
  (constructor:group-pattern subpatterns (falsey->option capture-key)))


(struct choice-pattern regular-pattern (choices)
  #:transparent
  #:name struct-transformer:choice-pattern
  #:constructor-name constructor:choice-pattern
  #:guard (λ (choices _) (sequence->vector choices)))


(define (choice-pattern choices)
  (constructor:choice-pattern choices))


(struct repetition-pattern regular-pattern (subpattern min-count max-count greedy?)
  #:transparent
  #:name struct-transformer:repetition-pattern
  #:constructor-name constructor:repetition-pattern)


(define (repetition-pattern subpattern
                            #:min-count [min-count 0]
                            #:max-count [max-count +inf.0]
                            #:greedy? [greedy? #true])
  (constructor:repetition-pattern subpattern min-count max-count greedy?))


(define (optional-pattern subpattern #:greedy? [greedy? #true])
  (repetition-pattern subpattern #:min-count 0 #:max-count 1 #:greedy? greedy?))


(struct lookahead-pattern regular-pattern (subpattern) #:transparent)


(define (regular-pattern-compile pattern)
  (define instructions (make-vector-builder))
  (define labels (make-hash))
  (define instruction-counter 0)
  (define label-counter 0)

  (define (next-label!)
    (define next label-counter)
    (set! label-counter (add1 next))
    next)

  (define (label! key)
    (hash-set! labels key instruction-counter))

  (define (add-instruction! instruction)
    (vector-builder-add instructions instruction)
    (set! instruction-counter (add1 instruction-counter)))

  (let loop ([pattern pattern] [peeking? #false])
      (match pattern

        [(element-pattern expected)
         (add-instruction! (if peeking? (peek-instruction expected) (read-instruction expected)))]

        [(lookahead-pattern subpattern)
         (loop subpattern #true)
         (add-instruction! (reset-peek-instruction))]

        [(struct-transformer:group-pattern subpatterns (== absent))
         (for ([subpattern (in-vector subpatterns)])
           (loop subpattern peeking?))]

        [(struct-transformer:group-pattern subpatterns (present key))
         (add-instruction! (start-group-instruction key))
         (loop (group-pattern subpatterns) peeking?)
         (add-instruction! (finish-group-instruction key))]

        [(struct-transformer:choice-pattern choices)
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

        [(struct-transformer:repetition-pattern subpattern 0 +inf.0 greedy?)
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

        [(struct-transformer:repetition-pattern subpattern 0 m greedy?)
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

        [(struct-transformer:repetition-pattern subpattern n m greedy?)
         #:when (> n 0)
         (for ([_ (in-range n)])
           (loop subpattern peeking?))
         (loop (repetition-pattern subpattern #:max-count (- m n) #:greedy? greedy?) peeking?)]))

    (add-instruction! (match-instruction))

  (compiled-regex-with-labels (build-vector instructions) labels))


(module+ test
  (test-case (name-string regular-pattern-compile)

    (test-case (name-string element-pattern)
      (define pattern (element-pattern #\a))
      (define expected (compiled-regex (list (read-instruction #\a) (match-instruction))))
      (check-equal? (regular-pattern-compile pattern) expected))

    (define a (element-pattern #\a))
    (define b (element-pattern #\b))
    (define c (element-pattern #\c))

    (test-case (name-string group-pattern)

      (test-case "non-capturing"
        (define pattern (group-pattern (list a b c)))
        (define expected
          (compiled-regex
           (list
            (read-instruction #\a)
            (read-instruction #\b)
            (read-instruction #\c)
            (match-instruction))))
        (check-equal? (regular-pattern-compile pattern) expected))

      (test-case "capturing"
        (define pattern (group-pattern (list a b c) #:capture-key 'foo))
        (define expected
          (compiled-regex
           (list
            (start-group-instruction 'foo)
            (read-instruction #\a)
            (read-instruction #\b)
            (read-instruction #\c)
            (finish-group-instruction 'foo)
            (match-instruction))))
        (check-equal? (regular-pattern-compile pattern) expected)))

    (test-case (name-string choice-pattern)
      (define pattern (choice-pattern (list a b c)))
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
          (match-instruction))))
      (check-equal? (regular-pattern-compile pattern) expected))

    (test-case (name-string element-string-pattern)
      (define expected (group-pattern (list a b c)))
      (check-equal? (element-string-pattern (list #\a #\b #\c)) expected)
      (check-equal? (element-string-pattern (list #\a #\b #\c)) expected)
      (check-equal? (element-string-pattern "abc") expected))

    (test-case (name-string element-set-pattern)
      (define expected (choice-pattern (list a b c)))
      (check-equal? (element-set-pattern (list #\a #\b #\c)) expected)
      (check-equal? (element-set-pattern (list #\a #\b #\c)) expected)
      (check-equal? (element-set-pattern "abc") expected)
      (check-equal? (element-set-pattern "aabbcc") expected)
      (check-equal? (element-set-pattern "abcabc") expected)
      (check-equal? (element-set-pattern "abccba") expected))

    (define abc (element-string-pattern "abc"))

    (test-case (name-string repetition-pattern)

      (test-case "greedy without quantifiers"
        (define pattern (repetition-pattern abc))
        (define expected
          (compiled-regex
           (list
            (split-instruction 1 5)
            (read-instruction #\a)
            (read-instruction #\b)
            (read-instruction #\c)
            (jump-instruction 0)
            (match-instruction))))
        (check-equal? (regular-pattern-compile pattern) expected))

      (test-case "non-greedy without quantifiers"
        (define pattern (repetition-pattern abc #:greedy? #false))
        (define expected
          (compiled-regex
           (list
            (split-instruction 5 1)
            (read-instruction #\a)
            (read-instruction #\b)
            (read-instruction #\c)
            (jump-instruction 0)
            (match-instruction))))
        (check-equal? (regular-pattern-compile pattern) expected))

      (test-case "greedy with minimum quantity"
        (define pattern (repetition-pattern abc #:min-count 3))
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
            (match-instruction))))
        (check-equal? (regular-pattern-compile pattern) expected))

      (test-case "non-greedy with minimum quantity"
        (define pattern (repetition-pattern abc #:min-count 3 #:greedy? #false))
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
            (match-instruction))))
        (check-equal? (regular-pattern-compile pattern) expected))

      (test-case "greedy with maximum quantity"
        (define pattern (repetition-pattern abc #:max-count 3))
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
            (match-instruction))))
        (check-equal? (regular-pattern-compile pattern) expected))

      (test-case "non-greedy with maximum quantity"
        (define pattern (repetition-pattern abc #:max-count 3 #:greedy? #false))
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
            (match-instruction))))
        (check-equal? (regular-pattern-compile pattern) expected))

      (test-case "greedy with minimum and maximum quantity"
        (define pattern (repetition-pattern abc #:min-count 3 #:max-count 5))
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
            (match-instruction))))
        (check-equal? (regular-pattern-compile pattern) expected))

      (test-case "non-greedy with minimum and maximum quantity"
        (define pattern (repetition-pattern abc #:min-count 3 #:max-count 5 #:greedy? #false))
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
            (match-instruction))))
        (check-equal? (regular-pattern-compile pattern) expected)))

    (test-case (name-string optional-pattern)

      (test-case "greedy"
        (define pattern (optional-pattern abc))
        (define expected
          (compiled-regex
           (list
            (split-instruction 1 4)
            (read-instruction #\a)
            (read-instruction #\b)
            (read-instruction #\c)
            (match-instruction))))
        (check-equal? (regular-pattern-compile pattern) expected))

      (test-case "non-greedy"
        (define pattern (optional-pattern abc #:greedy? #false))
        (define expected
          (compiled-regex
           (list
            (split-instruction 4 1)
            (read-instruction #\a)
            (read-instruction #\b)
            (read-instruction #\c)
            (match-instruction))))
        (check-equal? (regular-pattern-compile pattern) expected)))


    (test-case (name-string lookahead-pattern)
      (define pattern (lookahead-pattern abc))
      (define expected
        (compiled-regex
         (list
          (peek-instruction #\a)
          (peek-instruction #\b)
          (peek-instruction #\c)
          (reset-peek-instruction)
          (match-instruction))))
      (check-equal? (regular-pattern-compile pattern) expected))))
