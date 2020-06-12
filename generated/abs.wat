;; 
;; (define (abs x)
;; 	(if (>= x 0) 1 0))
;;
(module
    (func $abs (param $x i32)  (result i32)
        get_local $x
        i32.const 0
        i32.le_s
        (if (result i32)
            (then
                (i32.const 1)
            )
            (else
                (i32.const 0)
            )
        )

    )
    (export "dummy" (func $abs))

)
