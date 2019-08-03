#!r6rs



(import (rnrs lists (6))
        (rnrs base (6))
        (rnrs io simple (6)))
(display (find even? '(3 1 4 1 5 9)))




(define a 3)
(define b 4)
(define c (+ a b))
(display c)