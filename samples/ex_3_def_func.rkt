
(display (
  (lambda (a b)
    (define summator
      (lambda (x y) (+ x y)))
    (summator a b))
  111 222))
