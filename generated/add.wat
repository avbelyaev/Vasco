;;
;; (define (add x y)
;; 	(+ x y))
;;
(module
    (func $add (param $x i32) (param $y i32)  (result i32)
        get_local $x
        get_local $y
        i32.add
    )
    (export "dummy" (func $add))

)
