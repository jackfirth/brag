#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [port-shift-apparent-position (-> input-port? exact-nonnegative-integer? input-port?)]))


;@----------------------------------------------------------------------------------------------------


(define (port-shift-apparent-position in amount)
  (make-input-port (object-name in)
                   in
                   in
                   (λ () (close-input-port in))
                   (and (port-provides-progress-evts? in) (λ () (port-progress-evt in)))
                   (and (port-provides-progress-evts? in)
                        (λ (amt progress evt) (port-commit-peeked amt progress evt in)))
                   (λ ()
                     (define-values (line col pos) (port-next-location in))
                     (values line col (+ pos amount)))
                   (λ () (port-count-lines! in))
                   (+ (file-position in) amount 1)
                   (case-lambda
                     [() (file-stream-buffer-mode in)]
                     [(mode) (file-stream-buffer-mode in mode)])))
