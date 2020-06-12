;;
;; (define (identity x)
;; 	x)
;;
(module
(func $identity (param $x i32)  (result i32)
get_local $x

)
(export "dummy" (func $identity))

)
