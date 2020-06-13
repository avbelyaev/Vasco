#lang racket

; this one probably has to be desugared before parse

(define (area x) (* pi 2 x x))
(define pi 3.14)
(display (area 10))
(define (foo x)
    (define (identity y) y)
    (cond
        [(< x 0)  (error "fail!")]
        [(= x 0)  1]
        [(> x 0)  (+ x (foo (- x 1)))]
        [else     'universe-broke]))

(define bar 20)

(display (foo bar))

