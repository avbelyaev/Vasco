#lang racket


(define test 
	'((define (my-abs1 x) (if (< x 0) (-x) (x)))
	  (define (my-abs2 x) (if (< x 0) (-x) (x)))))
	  ;(display "my-abs")
	  ;(my-abs 3)))




(define (desugar-top-level expr)
	(match expr
		[`(define (,f ,params ...) . ,body) `(define ,f (lambda ,params . ,body))]))

(define (desugar-program exprs)
	(map desugar-top-level exprs))


(define (desugar prog)
	(set! prog (desugar-program prog))
	prog)

	
(display "desugaring...\n")
(define desugared (desugar test))

; pretty-format formats and converts ot str
(display (pretty-format desugared))
(newline)