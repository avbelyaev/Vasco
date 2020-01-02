
(define (iterate f x n)
  (if (zero? n)
      '()
    (cons x (iterate f (f x) (- n 1)))))


(display "iterate(x -> 2*x, 1, 6):")
(iterate (lambda (x) (* 2 x)) 1 6)
