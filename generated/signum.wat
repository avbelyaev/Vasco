;; 
;; (define (signum n)
;; 	(if (< n 0)
;;         -1
;;         (if (= n 0) 0 1)))
;;
(module
    (func $signum (param $n i32)  (result i32)
        get_local $n
        i32.const 0
        i32.lt_s
        (if (result i32)
            (then
                i32.const -1
            )
            (else
                get_local $n
                i32.const 0
                i32.eq
                (if (result i32)
                    (then
                        i32.const 0
                    )
                    (else
                        i32.const 1
                    )
                )
            )
        )
    )
    (export "dummy" (func $signum))

)
