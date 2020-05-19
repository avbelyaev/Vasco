#lang racket

(define (foo x)
    (define (identity y) y)
    (cond
        [(< x 0)  (error "fail!")]
        [(= x 0)  1]
        [(> x 0)  (+ x (foo (- x 1)))]
        [else     'universe-broke]))

(define bar 20)

(foo bar)

