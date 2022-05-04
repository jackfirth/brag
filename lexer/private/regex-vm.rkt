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
         syntax/parse/define)


(module+ test
  (require (submod "..")
           rackunit
           rebellion/private/static-name))


;@----------------------------------------------------------------------------------------------------


;; Based on the "regular expression virtual machine" implementation strategy described in
;; https://swtch.com/~rsc/regexp/regexp2.html.


(define (hash->immutable-hash h)
  (if (and (hash? h) (immutable? h))
      h
      (for/hash ([(k v) (in-hash h)])
        (values k v))))


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
  #:guard (λ (instructions _) (sequence->vector instructions)))


(struct regex-instruction () #:transparent)
(struct read-instruction regex-instruction (expected-char) #:transparent)
(struct peek-instruction regex-instruction (expected-char) #:transparent)
(struct reset-peek-instruction regex-instruction () #:transparent)
(struct jump-instruction regex-instruction (address) #:transparent)
(struct split-instruction regex-instruction (primary-address secondary-address) #:transparent)
(struct labeled-jump-instruction regex-instruction (label) #:transparent)
(struct labeled-split-instruction regex-instruction (primary-label secondary-label) #:transparent)
(struct match-instruction regex-instruction (mode) #:transparent)
(struct save-instruction regex-instruction (savepoint) #:transparent)
(struct fail-instruction regex-instruction () #:transparent)


(define (compiled-regex-savepoint-count compiled)
  (transduce (compiled-regex-program compiled)
             (filtering save-instruction?)
             (mapping save-instruction-savepoint)
             #:into into-count))


(struct regex-execution-result (mode savepoints)
  #:transparent
  #:guard (λ (mode savepoints _) (values mode (sequence->vector savepoints))))


(struct regex-thread (program-counter savepoints) #:transparent)


(struct thread-list ([size #:mutable] threads-by-priority-order threads-by-program-counter)
  #:transparent)


(define (make-thread-list #:program-size capacity)
  (thread-list 0 (make-vector capacity #false) (make-vector capacity #false)))


(define (thread-list-get threads index)
  (vector-ref (thread-list-threads-by-priority-order threads) index))


(define (thread-list-add! threads thread #:program program #:input input #:input-index i)
  (let loop ([thread thread] [i i] [pi i])
    (define pc (regex-thread-program-counter thread))
    (define savepoints (regex-thread-savepoints thread))
    (match (vector-ref program pc)
      [(jump-instruction address) (loop (regex-thread address savepoints) i pi)]
      [(split-instruction primary secondary)
       (define secondary-savepoints (vector-copy savepoints))
       (loop (regex-thread primary savepoints) i pi)
       (loop (regex-thread secondary secondary-savepoints) i pi)]
      [(save-instruction savepoint)
       (vector-set! savepoints savepoint i)
       (loop (regex-thread (add1 pc) savepoints) i pi)]
      [(peek-instruction expected)
       (when (equal? (string-ref input pi) expected)
         (loop (regex-thread (add1 pc) savepoints) i (add1 pi)))]
      [(reset-peek-instruction) (loop (regex-thread (add1 pc) savepoints) i i)]
      [_
       (define by-pc (thread-list-threads-by-program-counter threads))
       (unless (vector-ref by-pc pc)
         (define size (thread-list-size threads))
         (vector-set! by-pc pc thread)
         (vector-set! (thread-list-threads-by-priority-order threads) size thread)
         (set-thread-list-size! threads (add1 size)))])))


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


(define (compiled-regex-match-string r str)
  (define strlen (string-length str))
  (define program (compiled-regex-program r))
  (define savepoint-count (compiled-regex-savepoint-count r))

  (define (make-thread [pc 0] [savepoints (make-vector savepoint-count #false)])
    (regex-thread pc savepoints))
  
  (define running-threads (make-thread-list #:program-size (vector-length program)))
  (define blocked-threads (make-thread-list #:program-size (vector-length program)))
  (thread-list-add! running-threads (make-thread) #:program program #:input str #:input-index 0)
  (for/fold ([running-threads running-threads]
             [blocked-threads blocked-threads]
             [last-match #false]
             #:result last-match)
            ([input-index (in-range 0 (add1 strlen))])
    (let loop ([i 0])
      (cond
        [(equal? i (thread-list-size running-threads))
         (thread-list-clear! running-threads)
         (values blocked-threads running-threads last-match)]
        [else
         (define thread (thread-list-get running-threads i))
         (define pc (regex-thread-program-counter thread))
         (define savepoints (regex-thread-savepoints thread))
         (match (vector-ref program pc)
           [(read-instruction c)
            (when (and (< input-index strlen) (equal? (string-ref str input-index) c))
              (define next-thread (make-thread (add1 pc) savepoints))
              (thread-list-add! blocked-threads
                                next-thread
                                #:program program
                                #:input str
                                #:input-index input-index))
            (loop (add1 i))]
           [(match-instruction mode)
            (thread-list-clear! running-threads)
            (values blocked-threads running-threads (regex-execution-result mode savepoints))])]))))


(module+ test

  (test-case (name-string match-instruction)
    (define r (compiled-regex (list (match-instruction 42))))
    (check-equal? (compiled-regex-match-string r "aaaaaaaa") (regex-execution-result 42 '()))
    (check-equal? (compiled-regex-match-string r "") (regex-execution-result 42 '())))

  (test-case (name-string read-instruction)

    (test-case "reading one character"
      (define r (compiled-regex (list (read-instruction #\a) (match-instruction 0))))
      (check-equal? (compiled-regex-match-string r "a") (regex-execution-result 0 '()))
      (check-equal? (compiled-regex-match-string r "aaa") (regex-execution-result 0 '()))
      (check-equal? (compiled-regex-match-string r "ab") (regex-execution-result 0 '()))
      (check-false (compiled-regex-match-string r "b"))
      (check-false (compiled-regex-match-string r ""))
      (check-false (compiled-regex-match-string r "ba")))

    (test-case "reading multiple characters"
      (define r
        (compiled-regex
         (list
          (read-instruction #\a)
          (read-instruction #\b)
          (read-instruction #\c)
          (match-instruction 0))))
      (check-equal? (compiled-regex-match-string r "abc") (regex-execution-result 0 '()))
      (check-equal? (compiled-regex-match-string r "abcxxx") (regex-execution-result 0 '()))
      (check-false (compiled-regex-match-string r "cba"))
      (check-false (compiled-regex-match-string r "a"))
      (check-false (compiled-regex-match-string r "ab"))
      (check-false (compiled-regex-match-string r "aaa"))
      (check-false (compiled-regex-match-string r "bbb"))
      (check-false (compiled-regex-match-string r "ccc"))
      (check-false (compiled-regex-match-string r "aabc"))))

  (test-case (name-string jump-instruction)
    (define r
      (compiled-regex
       (list
        (jump-instruction 2) (read-instruction #\a) (read-instruction #\b) (match-instruction 0))))
    (check-equal? (compiled-regex-match-string r "b") (regex-execution-result 0 '()))
    (check-false (compiled-regex-match-string r "a"))
    (check-false (compiled-regex-match-string r "c")))

  (test-case (name-string split-instruction)
    (define r
      (compiled-regex
       (list
        (split-instruction 1 3)
        (read-instruction #\a)
        (match-instruction 0)
        (read-instruction #\b)
        (match-instruction 1))))
    (check-equal? (compiled-regex-match-string r "a") (regex-execution-result 0 '()))
    (check-equal? (compiled-regex-match-string r "b") (regex-execution-result 1 '()))
    (check-equal? (compiled-regex-match-string r "ab") (regex-execution-result 0 '()))
    (check-equal? (compiled-regex-match-string r "ba") (regex-execution-result 1 '()))
    (check-false (compiled-regex-match-string r "c"))
    (check-false (compiled-regex-match-string r "")))

  (test-case (name-string save-instruction)
    (define r
      (compiled-regex
       (list
        (save-instruction 0)
        (read-instruction #\a)
        (read-instruction #\a)
        (read-instruction #\a)
        (save-instruction 1)
        (read-instruction #\b)
        (read-instruction #\b)
        (read-instruction #\b)
        (save-instruction 2)
        (match-instruction 0))))
    (check-equal? (compiled-regex-match-string r "aaabbb") (regex-execution-result 0 '(0 2 5))))

  (test-case (name-string peek-instruction)
    (define r
      (compiled-regex
       (list
        (save-instruction 0)
        (peek-instruction #\a)
        (peek-instruction #\b)
        (peek-instruction #\c)
        (save-instruction 1)
        (match-instruction 0))))
    (check-equal? (compiled-regex-match-string r "abc") (regex-execution-result 0 '(0 0))))

  (test-case (name-string reset-peek-instruction)
    (define r
      (compiled-regex
       (list
        (save-instruction 0)
        (peek-instruction #\a)
        (peek-instruction #\b)
        (reset-peek-instruction)
        (peek-instruction #\a)
        (save-instruction 1)
        (match-instruction 0))))
    (check-equal? (compiled-regex-match-string r "abc") (regex-execution-result 0 '(0 0)))))
