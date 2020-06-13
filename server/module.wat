;; 
;; (define (fib n)
;; 	(if (<= n 2)
;; 		1
;; 		(+ (fib (- n 1)) (fib (- n 2)))))
;;
(module
    (func $fib (param $n i32)  (result i32)
        get_local $n
        i32.const 2
        i32.le_s
        (if (result i32)
            (then
                i32.const 1
            )
            (else
                get_local $n
                i32.const 1
                i32.sub

                call $fib
                get_local $n
                i32.const 2
                i32.sub

                call $fib
                i32.add
            )
        )
    )
    (export "dummy" (func $fib))

)
