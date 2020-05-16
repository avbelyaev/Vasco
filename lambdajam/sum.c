#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include "scheme.h"


Value __nil ;
Value __is_symbol ;
Value __is_pair ;
Value __cons ;
Value __car ;
Value __cdr ;
Value __is_eq ;
Value __is_proc ;
Value __or ;
Value __lt ;
Value __sum ;
Value __difference ;
Value __product ;
Value __display ;
Value __numEqual ;

struct __env_0 {
} ;

struct __env_0* __alloc_env0(){
  struct __env_0* t = malloc(sizeof(struct __env_0));
  return t;
}


Value __prim_is_symbol(Value e, Value a) {
  return MakeBoolean(a.t==SYMBOL);
}
Value __prim_is_pair(Value e, Value a) {
  return MakeBoolean(a.t==CONS);
}
Value __prim_cons(Value e, Value a, Value b) {
  return MakeCons(a, b);
}
Value __prim_car(Value e, Value a) {
  assert(a.t==CONS);
  return *a.cons.car;
}
Value __prim_cdr(Value e, Value a) {
  assert(a.t==CONS);
  return *a.cons.cdr;
}

Value __prim_is_proc(Value e, Value a, Value b) {
  return MakeBoolean(a.t==CLOSURE) ;
}
Value __prim_or(Value e, Value a, Value b) {
  assert(a.t==BOOLEAN);
  assert(b.t==BOOLEAN);
  return MakeBoolean(a.b.value || b.b.value) ;
}
Value __prim_lt(Value e, Value a, Value b) {
  assert(a.t==INT);
  assert(b.t==INT);
  return MakeBoolean(a.z.value < b.z.value) ;
}
Value __prim_sum(Value e, Value a, Value b) {
  assert(a.t==INT);
  assert(b.t==INT);
  return MakeInt(a.z.value + b.z.value) ;
}
Value __prim_product(Value e, Value a, Value b) {
  assert(a.t==INT);
  assert(b.t==INT);
  return MakeInt(a.z.value * b.z.value) ;
}
Value __prim_difference(Value e, Value a, Value b) {
  assert(a.t==INT);
  assert(b.t==INT);
  return MakeInt(a.z.value - b.z.value) ;
}
Value __prim_display(Value e, Value v) {
  print_value_ln(v);
  return v ;
}
Value __prim_numEqual(Value e, Value a, Value b) {
  assert(a.t==INT);
  assert(b.t==INT);
  return MakeBoolean(a.z.value == b.z.value) ;
}
Value __lambda_0() ;

Value __lambda_0(Value env_731, Value a, Value b) {
  Value tmp_734 ; 
  return (tmp_734 = __sum,tmp_734.clo.lam(MakeEnv(tmp_734.clo.env),a, b)) ;
}

int main (int argc, char* argv[]) {
  Value tmp_732 ; 
  Value tmp_733 ; 
  __nil         = MakeNil(); 
  __is_symbol   = MakePrimitive(__prim_is_symbol) ;
  __is_pair     = MakePrimitive(__prim_is_pair) ;
  __cons        = MakePrimitive(__prim_cons) ;
  __car         = MakePrimitive(__prim_car) ;
  __cdr         = MakePrimitive(__prim_cdr) ;
  __is_proc     = MakePrimitive(__prim_is_proc) ;
  __or          = MakePrimitive(__prim_or) ;
  __lt          = MakePrimitive(__prim_lt) ;
  __sum         = MakePrimitive(__prim_sum) ;
  __product     = MakePrimitive(__prim_product) ;
  __difference  = MakePrimitive(__prim_difference) ;
  __display     = MakePrimitive(__prim_display) ;
  __numEqual    = MakePrimitive(__prim_numEqual) ;
  (tmp_732 = __display,tmp_732.clo.lam(MakeEnv(tmp_732.clo.env),(tmp_733 = MakeClosure(__lambda_0,MakeEnv(__alloc_env0())),tmp_733.clo.lam(MakeEnv(tmp_733.clo.env),MakeInt(111), MakeInt(222))))) ;
  return 0;
 }

