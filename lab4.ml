open Util (* see util.ml *)

(******************)
(** Starter Code **)
(******************)

(* Red-Black Tree Data Type *)
type data = int
type color = Red | Black
type rbtree =
  | Empty
  | Rnode of rbtree * data * rbtree (* Red node *)
  | Bnode of rbtree * data * rbtree (* Black node *)

(* Return the color of an rbtree node *)
let rbt_color t =
  match t with
    Rnode(_,_,_) -> Red
  | Bnode(_,_,_) | Empty -> Black

(* Convert tree to string *)
let rec str_rbtree t =
  let h s l d r =
    Printf.sprintf "%s(%s,%d,%s)"
      s (str_rbtree l) d (str_rbtree r)
  in
  match t with
  | Empty -> "Empty"
  | Rnode(l,d,r) ->
     h "Rnode" l d r
  | Bnode(l,d,r) ->
     h "Bnode" l d r

(* Test if t satisfies the red-black tree invariants.
 *
 *  1. Does every red node have only black children?
 *  2. Does every path from root to leaf have the same number of black nodes?
 *)
let rbt_is_invariant (t:rbtree) : bool =
  (* TODO *)
  let rec h (t : rbtree) (x : int) =
    match t with
    | Empty -> true
    | Rnode(l, d, r) ->
      if rbt_color l == Red then false
      else if rbt_color r == Red then false
      else if h l x then true
      else h r x
    | Bnode(l, d, r) ->
      if h l (x+1) then h r (x+1)
      else false
  in
  match (h t 0) with
  | false -> false
  | true -> true



(* Test if red-black tree t is sorted. *)
  let rec rbt_is_sorted (t:rbtree) : bool =
    match t with
    | Empty -> true
    | Bnode(l, d, r) -> (match (l, r) with
                            | Empty, Empty -> true
                            | Bnode(ll, datal, rl), Bnode(lr, datar, rr) -> (rbt_is_sorted r) && (rbt_is_sorted l) && (datal < d) && (d < datar)
                            | Bnode(ll, datal, rl), Rnode(lr, datar, rr) -> (rbt_is_sorted r) && (rbt_is_sorted l) && (datal < d) && (d < datar)
                            | Rnode(ll, datal, rl), Bnode(lr, datar, rr) -> (rbt_is_sorted r) && (rbt_is_sorted l) && (datal < d) && (d < datar)
                            | Rnode(ll, datal, rl), Rnode(lr, datar, rr) -> (rbt_is_sorted r) && (rbt_is_sorted l) && (datal < d) && (d < datar))
    | Rnode(l, d, r) -> (match (l, r) with
                            | Empty, Empty -> true
                            | Bnode(ll, datal, rl), Bnode(lr, datar, rr) -> (rbt_is_sorted r) && (rbt_is_sorted l) && (datal < d) && (d < datar)
                            | Bnode(ll, datal, rl), Rnode(lr, datar, rr) -> (rbt_is_sorted r) && (rbt_is_sorted l) && (datal < d) && (d < datar)
                            | Rnode(ll, datal, rl), Bnode(lr, datar, rr) -> (rbt_is_sorted r) && (rbt_is_sorted l) && (datal < d) && (d < datar)
                            | Rnode(ll, datal, rl), Rnode(lr, datar, rr) -> (rbt_is_sorted r) && (rbt_is_sorted l) && (datal < d) && (d < datar))



(* Search for element x in red-black tree t.
 *
 * Return true if the tree contains x and false if it does not. *)
let rec rbt_search (t:rbtree) (x:data) : bool =
  (* TODO *)
  match t with
  | Empty -> false
  | Rnode (ltree, d, rtree) -> 
    if x > d then rbt_search rtree x
    else if x < d then rbt_search ltree x
    else true
  | Bnode (ltree, d, rtree) -> 
    if x > d then rbt_search rtree x
    else if x < d then rbt_search ltree x
    else true

(* Balance constructor for a red-black tree *)
let rbt_balance (c:color) (l:rbtree) (v:data) (r:rbtree) : rbtree =
  match (c,l,v,r) with
  (* TODO *)
  | (Black, Rnode(Rnode(a, x, b), y, g), z, e)
  | (Black, Rnode(a, x, Rnode(b, y, g)), z, e)
  | (Black, a, x, Rnode(Rnode(b, y, g), z, e))
  | (Black, a, x, Rnode(b, y, Rnode(g, z, e)))
  -> Rnode(Bnode(a, x, b), y, Bnode(g, z, e))
  | (Red, _, _, _) ->
    Rnode(l, v, r)
  | (Black, _, _, _) ->
    Bnode(l, v, r)

(* Insert element x into a red-black tree
 *
 * Do not reinsert (duplicate) existing elements *)
let rbt_insert (t:rbtree) (x:data) : rbtree =
  (* TODO *)
  let rec h (t : rbtree) =
    match t with
    | Empty -> Rnode(Empty, x, Empty)
    | Rnode(l, d, r) ->
      if x < d then rbt_balance Red (h l) d r
      else if x > d then rbt_balance Red l d (h r)
      else t
    | Bnode(l, d, r) ->
      if x < d then rbt_balance Red (h l) d r
      else if x > d then rbt_balance Red l d (h r)
      else t
  in
  match (h t) with
  | Rnode(l, x, r) -> Bnode(l, x, r)
  | Bnode(l, x, r) -> Bnode(l, x, r)
  | Empty -> Empty

(***********)
(** Tests **)
(***********)

(* See description in testing.ml *)

let tree_arg_printer =
  (fun (t,x) -> str_pair str_rbtree string_of_int (t,x))

exception InvalidRBTreeError

(* To check that test case inputs are valid, sorted red-black trees *)
let check_rbtree t =
  if (rbt_is_invariant t) && (rbt_is_sorted t) then t
  else raise InvalidRBTreeError

(* To check that test case expected outputs are valid, sorted red-black trees *)
let eq_rbtree t1 t_expected =
  t1 = (check_rbtree t_expected)

(* Leaf node constants to make test inputs more readable *)
let r0 =  Rnode(Empty, 0,Empty)
let r1 =  Rnode(Empty, 1,Empty)
let r2 =  Rnode(Empty, 2,Empty)
let r3 =  Rnode(Empty, 3,Empty)
let r4 =  Rnode(Empty, 4,Empty)
let r5 =  Rnode(Empty, 5,Empty)
let r6 =  Rnode(Empty, 6,Empty)
let r7 =  Rnode(Empty, 7,Empty)
let r8 =  Rnode(Empty, 8,Empty)
let r9 =  Rnode(Empty, 9,Empty)
let r10 = Rnode(Empty,10,Empty)
let r11 = Rnode(Empty,11,Empty)
let r12 = Rnode(Empty,12,Empty)
let r13 = Rnode(Empty,13,Empty)
let r14 = Rnode(Empty,14,Empty)
let r15 = Rnode(Empty,15,Empty)
let r16 = Rnode(Empty,16,Empty)
let r17 = Rnode(Empty,17,Empty)
let r18 = Rnode(Empty,18,Empty)
let r19 = Rnode(Empty,19,Empty)
let r20 = Rnode(Empty,20,Empty)
let r21 = Rnode(Empty,21,Empty)
let r22 = Rnode(Empty,22,Empty)
let r23 = Rnode(Empty,23,Empty)
let r24 = Rnode(Empty,24,Empty)
let r25 = Rnode(Empty,25,Empty)
let r26 = Rnode(Empty,26,Empty)
let r27 = Rnode(Empty,27,Empty)
let r28 = Rnode(Empty,28,Empty)
let r29 = Rnode(Empty,29,Empty)
let r30 = Rnode(Empty,30,Empty)
let r31 = Rnode(Empty,31,Empty)
let r32 = Rnode(Empty,32,Empty)
let r33 = Rnode(Empty,33,Empty)
let r34 = Rnode(Empty,34,Empty)

let b0 =  Bnode(Empty, 0,Empty)
let b1 =  Bnode(Empty, 1,Empty)
let b2 =  Bnode(Empty, 2,Empty)
let b3 =  Bnode(Empty, 3,Empty)
let b4 =  Bnode(Empty, 4,Empty)
let b5 =  Bnode(Empty, 5,Empty)
let b6 =  Bnode(Empty, 6,Empty)
let b7 =  Bnode(Empty, 7,Empty)
let b8 =  Bnode(Empty, 8,Empty)
let b9 =  Bnode(Empty, 9,Empty)
let b10 = Bnode(Empty,10,Empty)
let b11 = Bnode(Empty,11,Empty)
let b12 = Bnode(Empty,12,Empty)
let b13 = Bnode(Empty,13,Empty)
let b14 = Bnode(Empty,14,Empty)
let b15 = Bnode(Empty,15,Empty)
let b16 = Bnode(Empty,16,Empty)
let b17 = Bnode(Empty,17,Empty)
let b18 = Bnode(Empty,18,Empty)
let b19 = Bnode(Empty,19,Empty)
let b20 = Bnode(Empty,20,Empty)

let rbt_is_invariant_tests =
  ("rbt_is_invariant",
   rbt_is_invariant,
   (=), (=),
   Some(str_rbtree,
        str_bool),
   [
     (Some("simple tree"),
      Bnode(r1, 2, r3),
      Ok(true));
     (* TODO *)
     (Some("simple tree with zero children"),
      b2,
      Ok(true));

     (Some("simple tree, two black nodes with red children"),
      Bnode(Bnode(r2, 12, Empty), 20, Bnode(Empty, 25, r26)),
      Ok(true));

     (Some("simple tree, 3 black nodes and unbalanced (left heavy)"),
      Bnode(Bnode(r2, 12, Bnode(Empty, 3, r5)), 20, Bnode(Empty, 25, r26)),
      Ok(false));

     (Some("simple tree, 3 black nodes and unbalanced (right heavy)"),
      Bnode(Bnode(r2, 12, Empty), 20, Bnode(Bnode(r22, 24, Empty), 25, r26)),
      Ok(false));

     (Some("empty tree"),
      Empty,
      Ok(true));
   ])

let rbt_is_sorted_tests =
  ("rbt_is_sorted",
   rbt_is_sorted,
   (=), (=),
   Some(str_rbtree,
        str_bool),
   [
     (Some("simple tree"),
      Bnode(r1, 2, r3),
      Ok(true));
     (* TODO *)
     (Some("simple tree with zero children"),
      b2,
      Ok(true));

     (Some("simple tree, two black nodes with red children"),
      Bnode(Bnode(r2, 12, Empty), 20, Bnode(Empty, 25, r26)),
      Ok(true)); 

     (Some("empty tree"),
      Empty,
      Ok(true));

      (*(Some("larger tree"),
      Bnode(Rnode(b1, 2, Empty), 3, Rnode(Empty, 4, b5),
      Ok(true)));*)

      (*(Some("incorrect sorted larger tree"),
      Bnode(Rnode(b1, 3, Empty), 4, Rnode(Empty, 4, b5),
      Ok(false)));*)
   ])

let rbt_search_tests =
  ("rbt_search",
   (fun (t,x) -> rbt_search t x),
   (=), (=),
   Some(tree_arg_printer,
        str_bool),
   [
     (Some("simple tree"),
      (Bnode(r1, 2, r3), 2),
      Ok(true));
     (* TODO *)

     (Some("empty tree"),
     (Empty, 1),
     Ok(false));

     (Some("large tree"),
      (Bnode(Rnode(b1, 2, b3), 4, Rnode(b5, 6, b7)), 6),
      Ok(true));
     
      (Some("incorrect large tree w/ value"),
      (Bnode(Bnode(b1, 2, b3), 4, Rnode(b5, 6, b7)), 6),
      Ok(false));

      (Some("large tree w/o value"),
      (Bnode(Rnode(b1, 2, b3), 4, Rnode(b5, 6, b7)), 9),
      Ok(false));

      (Some("lopsided tree"),
      (Bnode(Rnode(b1, 2, b3), 4, Rnode(Empty, 5, Empty)), 5),
      Ok(true));
   ])

let rbt_balance_tester t =
  (* Note: rbt_balance does not always return a balanced tree!  We
     only enforce balance when we reach the black grandparent in a
     red-red invariant violation. *)
  match t with
    Empty -> raise InvalidRBTreeError (* we don't ever balance empty trees *)
  | Rnode(l,v,r) | Bnode(l,v,r)
    -> rbt_balance (rbt_color t) l v r

let rbt_balance_tests =
  ("rbt_balance",
   rbt_balance_tester,
   (=), (=),
   Some(str_rbtree, str_rbtree),
   [
     (Some("Case A"),
      Bnode(Rnode(r1,2,Empty),
            3,
            Empty),
      Ok(Rnode(b1,2,b3)));
     (* TODO *)

     (Some("No change"),
     Bnode(b1, 2, b3),
     Ok(Bnode(b1,2,b3)));

     (Some("simple red"),
      Rnode(r1, 2, Empty),
      Ok(Rnode(r1, 2, Empty)));

     (Some("Fix bigger red"),
     Bnode(Bnode(r1,2,Empty),
           4,
           Bnode(Empty, 5, Empty)),
     Ok(Bnode(Bnode(r1, 2, Empty),4, b5)));
   ])


let rbt_insert_tester (t,x) = rbt_insert t x

let rbt_insert_tests =
  ("rbt_insert",
   rbt_insert_tester,
   eq_rbtree, (=),
   Some(tree_arg_printer,
        str_rbtree),
   [
     (Some("simple tree"),
      (Bnode(r1, 2, Empty), 3),
      Ok(Bnode(r1, 2, r3)));
     (* TODO *)

     (Some("lopsided tree"),
      (Bnode(Empty, 2, r3), 4),
      Ok(Bnode(Empty,2,Rnode(Empty,3,Rnode(Empty,4,Empty)))));

     (Some("full simple tree"),
      (Bnode(r1, 2, r3), 4),
      Ok(Bnode(b1, 2, Bnode(Empty, 3, r4))));

     (Some("larger tree bigger value"),
      ((Bnode(b1, 2, Bnode(Empty, 3, r4))), 5),
      Ok(Bnode(b1, 2, Bnode(r3, 4, r5))));

     (Some("larger tree middle value"),
      ((Bnode(b1, 2, Bnode(r3, 5, r6))), 4),
      Ok(Bnode(b1, 2, Rnode(Bnode(Empty, 3, r4), 5, b6))));
   ])