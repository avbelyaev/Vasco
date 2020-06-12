#lang racket

(define pi 3.14)

(define (my-abs x)
  (if (< x 0) (- x) x))
(display "my-abs")
(my-abs 3)

(define (sq x) (* x x))

(define sq
  (lambda (x) (* x x)))

(define (summator x)
  (let ((y 123)
        (z 456))
    (+ x y z)))

(define (sign x)
  (cond
    ((< x 0) -1)
    ((= x 0) 0)
    (else 1)))

(let ((a (list 1 2 3))
      (b (list 4 5 6)))
      (cons a b))

(let* ((x 1)
       (y (- x 5)))
  (+ x y))

(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))


(define (iterate f x n)
  (if (zero? n)
      '()
    (cons x (iterate f (f x) (- n 1)))))


(display "iterate(x -> 2*x, 1, 6):")
(iterate (lambda (x) (* 2 x)) 1 6)


(define (sperse e xs)
  (if (null? xs)
      '()
    (if (eq? e (car xs))
      (cons (car xs) (sperse e (cdr xs)))
      (cons (car xs) (sperse e (cons e (cdr xs)))))))

(display "intersperse('x' [1, 2, 3, 4]):")
(sperse 'x '(1 2 3))


(define (replace pred proc xs)
  (if (null? xs)
      '()
      (cons (if (pred (car xs))
                (proc (car xs))
                (car xs))
            (replace pred proc (cdr xs)))))

(display "replace(even?, x -> x * 10, [0 1 2 3 4]):")
(replace even? (lambda (x) (* x 10)) '(0 1 2 3 4))


(define (ref seq index)
  (cond
    ((vector? seq) (if (and (< index (vector-length seq))
                         (>= index 0))
                     (vector-ref seq index)
                     #f))
    ((list? seq) (if (and (< index (length seq))
                       (>= index 0))
                   (list-ref seq index)
                   #f))
    ((string? seq) (if (and (< index (string-length seq))
                         (>= index 0))
                     (string-ref seq index)
                     #f))
    (else #f)))
