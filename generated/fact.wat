;; 
;; (define (fact n)
;; 	(if (= n 0)
;; 		1
;; 		(* n (fact (- n 1)))))
;;
(module
    (func $fact (param $n i32)  (result i32)
        get_local $n
        i32.const 0
        i32.eq
        (if (result i32)
            (then
                i32.const 1)
            (else
                get_local $n
                get_local $n
                i32.const 1
                i32.sub

                call $fact
                i32.mul
            )
        )

    )
    (export "dummy" (func $fact))

)
