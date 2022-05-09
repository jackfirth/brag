#lang racket/base


(provide
 (all-defined-out))


(require (for-syntax racket/base
                     syntax/parse)
         racket/match
         racket/vector
         rebellion/collection/vector
         rebellion/streaming/reducer
         rebellion/streaming/transducer
         syntax/parse/define
         yaragg/lexer/private/regular-match
         yaragg/private/hash)


(module+ test
  (require (submod "..")
           rackunit
           rebellion/private/static-name))


;@----------------------------------------------------------------------------------------------------


;; Based on the "regular expression virtual machine" implementation strategy described in
;; https://swtch.com/~rsc/regexp/regexp2.html.


(define (compiled-regex-with-labels program labels)
  (compiled-regex
   (for/vector ([instruction (in-vector program)])
     (match instruction
       [(labeled-jump-instruction label)
        (jump-instruction (hash-ref labels label))]
       [(labeled-split-instruction primary secondary)
        (split-instruction (hash-ref labels primary) (hash-ref labels secondary))]
       [other other]))))


(struct compiled-regex (program)
  #:transparent
  #:guard
  (λ (instructions _) (sequence->vector instructions)))


(struct regex-instruction () #:transparent)
(struct read-instruction regex-instruction (expected-char) #:transparent)
(struct peek-instruction regex-instruction (expected-char) #:transparent)
(struct reset-peek-instruction regex-instruction () #:transparent)
(struct jump-instruction regex-instruction (address) #:transparent)
(struct split-instruction regex-instruction (primary-address secondary-address) #:transparent)
(struct labeled-jump-instruction regex-instruction (label) #:transparent)
(struct labeled-split-instruction regex-instruction (primary-label secondary-label) #:transparent)
(struct match-instruction regex-instruction () #:transparent)
(struct start-group-instruction regex-instruction (key) #:transparent)
(struct finish-group-instruction regex-instruction (key) #:transparent)
(struct fail-instruction regex-instruction () #:transparent)


(struct regex-thread (program-counter captured-groups) #:transparent)


(struct thread-list ([size #:mutable] threads-by-priority-order threads-by-program-counter)
  #:transparent)


(define (make-thread-list #:program-size capacity)
  (thread-list 0 (make-vector capacity #false) (make-vector capacity #false)))


(define (thread-list-get threads index)
  (vector-ref (thread-list-threads-by-priority-order threads) index))


(define (thread-list-add! threads thread #:program program #:input input #:input-index i)
  (let loop ([thread thread] [i i] [peek i] [max-peek i])
    (define pc (regex-thread-program-counter thread))
    (define groups (regex-thread-captured-groups thread))
    (match (vector-ref program pc)
      [(jump-instruction address) (loop (regex-thread address groups) i peek max-peek)]
      [(split-instruction primary secondary)
       (define secondary-groups (captured-groups-builder-copy groups))
       (max (loop (regex-thread primary groups) i peek max-peek)
            (loop (regex-thread secondary secondary-groups) i peek max-peek))]
      [(start-group-instruction key)
       (captured-groups-builder-start-group! groups key i)
       (loop (regex-thread (add1 pc) groups) i peek max-peek)]
      [(finish-group-instruction key)
       (captured-groups-builder-finish-group! groups key i)
       (loop (regex-thread (add1 pc) groups) i peek max-peek)]
      [(peek-instruction expected)
       (cond
         [(equal? (string-ref input peek) expected)
          (define next-peek (add1 peek))
          (loop (regex-thread (add1 pc) groups) i next-peek (max max-peek next-peek))]
         [else max-peek])]
      [(reset-peek-instruction) (loop (regex-thread (add1 pc) groups) i i max-peek)]
      [_
       (define by-pc (thread-list-threads-by-program-counter threads))
       (unless (vector-ref by-pc pc)
         (define size (thread-list-size threads))
         (vector-set! by-pc pc thread)
         (vector-set! (thread-list-threads-by-priority-order threads) size thread)
         (set-thread-list-size! threads (add1 size)))
       max-peek])))


(define (thread-list-clear! threads)
  (define by-priority (thread-list-threads-by-priority-order threads))
  (define by-pc (thread-list-threads-by-program-counter threads))
  (for ([thread (in-vector by-priority 0 (thread-list-size threads))]
        [i (in-naturals)])
    (vector-set! by-pc (regex-thread-program-counter thread) #false)
    ;; TODO: this isn't strictly necessary (setting size to zero makes the by-priority vector's
    ;; contents unreadable anyway) but I'm leaving it in for now as a defense against other bugs, this
    ;; should be removed once tests are passing.
    (vector-set! by-priority i #false))
  (set-thread-list-size! threads 0))


(define (compiled-regex-match-string r str [start 0] [end (string-length str)])
  (define program (compiled-regex-program r))

  (define (make-thread [pc 0] [groups (make-captured-groups-builder)])
    (regex-thread pc groups))
  
  (define running-threads (make-thread-list #:program-size (vector-length program)))
  (define blocked-threads (make-thread-list #:program-size (vector-length program)))
  (define max-peek
    (thread-list-add! running-threads (make-thread) #:program program #:input str #:input-index start))
  (for/fold ([running-threads running-threads]
             [blocked-threads blocked-threads]
             [last-match #false]
             [max-peek max-peek]
             #:result (or last-match (regular-match-failure start (- max-peek start))))
            ([input-index (in-range start (add1 end))])
    (let loop ([i 0] [max-peek max-peek])
      (cond
        [(equal? i (thread-list-size running-threads))
         (thread-list-clear! running-threads)
         (values blocked-threads running-threads last-match max-peek)]
        [else
         (define thread (thread-list-get running-threads i))
         (define pc (regex-thread-program-counter thread))
         (define groups (regex-thread-captured-groups thread))
         (match (vector-ref program pc)
           [(read-instruction c)
            (cond
              [(and (< input-index end) (equal? (string-ref str input-index) c))
               (define next-thread (make-thread (add1 pc) groups))
               (define next-max-peek
                 (thread-list-add! blocked-threads
                                   next-thread
                                   #:program program
                                   #:input str
                                   #:input-index (add1 input-index)))
               (loop (add1 i) (max max-peek next-max-peek))]
              [else
               (loop (add1 i) (max (add1 input-index) max-peek))])]
           [(match-instruction)
            (thread-list-clear! running-threads)
            (define resulting-match
              (regular-match start input-index
                             #:peek-distance (- max-peek input-index)
                             #:groups (build-captured-groups groups)))
            (values blocked-threads running-threads resulting-match max-peek)])]))))


(module+ test

  (test-case (name-string match-instruction)
    (define r (compiled-regex (list (match-instruction))))
    (define expected (regular-match 0 0))
    (check-equal? (compiled-regex-match-string r "aaaaaaaa") expected)
    (check-equal? (compiled-regex-match-string r "") expected))

  (test-case (name-string read-instruction)

    (test-case "reading one character"
      (define r (compiled-regex (list (read-instruction #\a) (match-instruction))))
      (define expected (regular-match 0 1))
      (check-equal? (compiled-regex-match-string r "a") expected)
      (check-equal? (compiled-regex-match-string r "aaa") expected)
      (check-equal? (compiled-regex-match-string r "ab") expected)
      (check-equal? (compiled-regex-match-string r "b") (regular-match-failure 0 1))
      (check-equal? (compiled-regex-match-string r "") (regular-match-failure 0 1))
      (check-equal? (compiled-regex-match-string r "ba") (regular-match-failure 0 1)))

    (test-case "reading multiple characters"
      (define r
        (compiled-regex
         (list
          (read-instruction #\a)
          (read-instruction #\b)
          (read-instruction #\c)
          (match-instruction))))
      (define expected (regular-match 0 3))
      (check-equal? (compiled-regex-match-string r "abc") expected)
      (check-equal? (compiled-regex-match-string r "abcxxx") expected)
      (check-equal? (compiled-regex-match-string r "cba") (regular-match-failure 0 1))
      (check-equal? (compiled-regex-match-string r "a") (regular-match-failure 0 2))
      (check-equal? (compiled-regex-match-string r "ab") (regular-match-failure 0 3))
      (check-equal? (compiled-regex-match-string r "aaa") (regular-match-failure 0 2))
      (check-equal? (compiled-regex-match-string r "bbb") (regular-match-failure 0 1))
      (check-equal? (compiled-regex-match-string r "ccc") (regular-match-failure 0 1))
      (check-equal? (compiled-regex-match-string r "aabc") (regular-match-failure 0 2))))

  (test-case (name-string jump-instruction)
    (define r
      (compiled-regex
       (list
        (jump-instruction 2) (read-instruction #\a) (read-instruction #\b) (match-instruction))))
    (check-equal? (compiled-regex-match-string r "b") (regular-match 0 1))
    (check-equal? (compiled-regex-match-string r "a") (regular-match-failure 0 1))
    (check-equal? (compiled-regex-match-string r "c") (regular-match-failure 0 1)))

  (test-case "group capturing instructions"
    (define r
      (compiled-regex
       (list
        (start-group-instruction 'a)
        (read-instruction #\a)
        (read-instruction #\a)
        (read-instruction #\a)
        (finish-group-instruction 'a)
        (start-group-instruction 'b)
        (read-instruction #\b)
        (read-instruction #\b)
        (read-instruction #\b)
        (finish-group-instruction 'b)
        (match-instruction))))
    (define expected
      (regular-match 0 6
                     #:groups (hash 'a (list (captured-group 0 3)) 'b (list (captured-group 3 6)))))
    (check-equal? (compiled-regex-match-string r "aaabbb") expected))

  (test-case (name-string split-instruction)
    (define r
      (compiled-regex
       (list
        (split-instruction 1 5)
        (start-group-instruction 'a)
        (read-instruction #\a)
        (finish-group-instruction 'a)
        (match-instruction)
        (start-group-instruction 'b)
        (read-instruction #\b)
        (finish-group-instruction 'b)
        (match-instruction))))
    (define a-match (regular-match 0 1 #:groups (hash 'a (list (captured-group 0 1)))))
    (define b-match (regular-match 0 1 #:groups (hash 'b (list (captured-group 0 1)))))
    (check-equal? (compiled-regex-match-string r "a") a-match)
    (check-equal? (compiled-regex-match-string r "b") b-match)
    (check-equal? (compiled-regex-match-string r "ab") a-match)
    (check-equal? (compiled-regex-match-string r "ba") b-match)
    (check-equal? (compiled-regex-match-string r "c") (regular-match-failure 0 1))
    (check-equal? (compiled-regex-match-string r "") (regular-match-failure 0 1)))

  (test-case (name-string peek-instruction)
    (define r
      (compiled-regex
       (list
        (start-group-instruction 'a)
        (peek-instruction #\a)
        (peek-instruction #\b)
        (peek-instruction #\c)
        (finish-group-instruction 'a)
        (match-instruction))))
    (define expected
      (regular-match 0 0 #:peek-distance 3 #:groups (hash 'a (list (captured-group 0 0)))))
    (check-equal? (compiled-regex-match-string r "abc") expected))

  (test-case (name-string reset-peek-instruction)
    (define r
      (compiled-regex
       (list
        (start-group-instruction 'a)
        (peek-instruction #\a)
        (peek-instruction #\b)
        (reset-peek-instruction)
        (peek-instruction #\a)
        (finish-group-instruction 'a)
        (match-instruction))))
    (define expected
      (regular-match 0 0 #:peek-distance 2 #:groups (hash 'a (list (captured-group 0 0)))))
    (check-equal? (compiled-regex-match-string r "abc") expected)))
