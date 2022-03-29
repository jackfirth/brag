#lang racket/base
(provide terminals-def terminals-def-t terminals-def?
         e-terminals-def e-terminals-def-t e-terminals-def?)

;; The things needed at compile time to handle definition of tokens
(struct terminals-def (t))
(struct e-terminals-def (t))
