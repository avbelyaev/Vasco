
(display ((lambda (x)
            (define (fact n)
              (if (= n 0)
                1
                (* n (fact (- n 1)))))
            (fact x))
           5))
