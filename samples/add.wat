;;
;; example from MDN: https://developer.mozilla.org/en-US/docs/WebAssembly/Understanding_the_text_format
;;
(module
  (func (param $lhs i32) (param $rhs i32) (result i32)
    local.get $lhs
    local.get $rhs
    i32.add))
