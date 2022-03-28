#lang racket/base

(module+ test

  (require yaragg/examples/start-and-atok
           yaragg/support
           rackunit)

  ;; make sure that "start" and "atok" work as terminals.

  (check-equal? (parse-to-datum (list "start")) '(top "start"))
  (check-equal? (parse-to-datum (list "atok")) '(top "atok"))
  (check-equal? (parse-to-datum (list "start" "atok")) '(top "start" "atok")))
