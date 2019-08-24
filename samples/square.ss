#lang racket


(define (my-abs x)
  (if (< x 0) (-x) (x)))
