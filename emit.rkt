#lang racket

 

; program being generated
; TODO: read from stdin
(define test
    '(
       closure*
        (lambda*
          (CLOSURE130 f)
          (closure*
            (lambda* (CLOSURE131 x) ((envGet CLOSURE131 f) x (envGet CLOSURE131 a)))
            (env* (a (envGet CLOSURE130 a)) (f f))))
          (env* (a a))
     ))


(define (generate line)
  (display line)
  (newline))


(define (generateCode exp)
  (generate "asdas"))




(define (main input)
  (displayln "generating code...")
  (pretty-write input)

  (generateCode input))



(main test)