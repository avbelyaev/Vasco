(module
  (func $id (param $x i32) (result i32)
    get_local $x
    )
  (export "tmp" (func $id))
)
