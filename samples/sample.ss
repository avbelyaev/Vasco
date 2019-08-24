#lang racket

(define (my-abs x) (if (< x 0) (-x) (x)))

(define (sign x)
  (cond
    ((< x 0) -1)
    ((= x 0) 0)
    (else 1)))


(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))

(define (replace pred proc xs)
  (if (null? xs)
      '()
      (cons (if (pred (car xs))
                (proc (car xs))
                (car xs))
            (replace pred proc (cdr xs)))))

(display "replace(even?, x -> x * 10, [0 1 2 3 4]):")
(replace even? (lambda (x) (* x 10)) '(0 1 2 3 4))
