#lang typed/racket

(define (fact [n : Integer]) : Integer
              (if (= n 0)
                1
                (* n (fact (- n 1)))))

(fact 6)