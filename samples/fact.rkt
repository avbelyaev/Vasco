#lang racket

(define factorial
  (lambda (n)
    (begin
      (sho n)
      (if (= n 0) 1
        (* n (factorial (- n 1)))))))

(display (factorial 5))