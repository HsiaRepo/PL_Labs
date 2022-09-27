open Util;;
open Javascript_ast;;


(* The heap is tuple containing
 * - a map from ints (addresses) to values
 * - the next "free" address
 *)
type heap_t = (value_t IntMap.t) * address_t

(* Assign a value to a reference in the heap *)
let heap_assign (h:heap_t) (addr:address_t) (value:value_t)  : heap_t =
  let m,i = h in
  IntMap.add addr value m,i

(* "Allocate" a new RefVal in the heap.  Store the provided value and return the RefVal. *)
let heap_alloc (h:heap_t) (v:value_t) : value_t*heap_t =
  let (m,i) = h in
  RefVal(i), heap_assign (m,i+1) i v

(* Deference a pointer in the heap *)
let heap_deref (h:heap_t) (addr:address_t)  : value_t =
  let m,_ = h in
  try IntMap.find addr m
  with _ -> raise (RefError(ValExpr(NoPos, RefVal addr)))


let empty_heap = (IntMap.empty,0)

let eq_value_heap_value (v1,h1 : value_t*heap_t) (v2,h2 : value_t*heap_t) =
  eq_value v1 v2

let str_heap (h:heap_t) =
  let (h,_) = h in
  "[" ^
    (str_x_list
       (fun (k,v) -> Printf.sprintf "%d->%s" k (str_value v))
       (IntMap.bindings h)
       ", ")
    ^ "]"
