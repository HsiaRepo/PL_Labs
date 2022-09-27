(* open Util (* see util.ml *)

(******************)
(** Starter Code **)
(******************)

(* Binary Tree Data Type *)
type data = int
type binary_tree =
  | Empty
  | Node of binary_tree * data * binary_tree


(* Convert tree to string *)
let rec str_binary_tree t =
  match t with
  | Empty -> "Empty"
  | Node(l,d,r) ->
     Printf.sprintf "Node(%s,%d,%s)"
       (str_binary_tree l)
       d
       (str_binary_tree r)


(* Higher Order Functions on Binary Trees *)

(* Apply function f to every data element of tree t and collect the
   results in a list following an inorder traversal of the tree *)
let map_inorder (f : data -> 'a) (t : binary_tree) : 'a list =
  (* TODO *)
  let rec h (t : binary_tree) (a : 'a list) : 'a list =
    match t with
    | Empty -> a
    | Node(l, d, r)->
      let a' = h r a
      in let d' = f d
      in h l (d'::a')
  in h t []


(* Apply function f to every data element of tree t and collect the
   results in a list following a reverse order traversal of the tree *)
let map_revorder (f : data -> 'a) (t : binary_tree) : 'a list =
  (* TODO *)
  let rec h (t : binary_tree) (a : 'a list) : 'a list =
    match t with
    | Empty -> a
    | Node(l, d, r)->
      let a' = h l a
      in let d' = f d
      in h r (d'::a')
  in h t []


(* Binary Search Trees *)


(* Test if t is a binary search tree.
 *
 * That is, (recursively) are all elements to the left less the the
 * current element and all elements to the right greater than the
 * current element *)
let is_bst (t : binary_tree) : bool =
  let rec check tree = 
    match tree with
    | Empty -> true
    | Node(l, y, r) -> 
      match (l, r) with
        | (Empty, Empty) -> true
        | (Node(_, lx, _), Empty) -> if y < lx then false else check l
        | (Empty, Node(_, rx, _)) -> if y > rx then false else check r
        | (Node(_, lx, _), Node(_, rx, _)) -> if (y < lx || y > rx) then false else (check l && check r)
      in check t
    
  
(* Return the maximum element of a binary search tree. *)
let rec bst_max (t : binary_tree) : data option =
  (* TODO *)
  match t with
  | Empty -> None
  | Node(l, d, r) ->
    match r with
    | Empty -> Some d
    | Node(l2, d2, r2) -> bst_max r

(* Return the minimum element of a binary search tree. *)
let rec bst_min t : data option =
  (* TODO *)
  match t with
  | Empty -> None
  | Node(l, d, r) ->
    match l with
    | Empty -> Some d
    | Node(l2, d2, r2) -> bst_min l


(* Insert element x into binary search tree t.
 *
 * Do not reinsert (duplicate) existing elements *)
let rec bst_insert (t : binary_tree) (x : data) : binary_tree =
  (* TODO *)
  match t with 
  | Empty -> Node(Empty, x, Empty)
  | Node(l, d, r) -> 
    if x == d then t
    else if x < d then Node(bst_insert l x, d, r)
    else Node(l, d, bst_insert r x)

      

(* Search for element x in binary search tree t.
 *
 * Return true if the tree contains x and false if it does not. *)
let rec bst_search (t : binary_tree) (x : data) : bool =
  match t with
  | Empty -> false
  | Node (l,y,r) -> if x > y then bst_search r x
      else if x < y then bst_search l x
      else true

(* Remove the minimum element of binary search tree t.
 *
 * Return a tuple containing the new binary tree and an option of the
 * removed element.  *)
let rec bst_remove_min (t : binary_tree) : binary_tree * data option =
  (* TODO *)
  (t, None)

(* Remove the element x from binary search tree t
 *
 * Return a tuple containing the new binary tree and a bool that is
 * true if the element was found and false if the element was not
 * found *)
let rec bst_remove (t : binary_tree) (x : data) : binary_tree * bool =
  (* TODO *)
  (t, false)

(***********)
(** Tests **)
(***********)

(* See description in testing.ml *)

let map_printer =
  Some((fun (f,t) -> str_binary_tree t),
       str_int_list)

let tree_arg_printer =
  (fun (t,x) -> str_pair str_binary_tree string_of_int (t,x))

let l0 = Node(Empty,0,Empty)
let l1 = Node(Empty,1,Empty)
let l2 = Node(Empty,2,Empty)
let l3 = Node(Empty,3,Empty)
let l4 = Node(Empty,4,Empty)
let l5 = Node(Empty,5,Empty)

let l6 = Node(l0, 6, l1)
let l7 = Node(l2, 7, l3)

let identity (x:int) : int = x

let map_inorder_tests =
  ("map_inorder",
   (fun (f,t) -> map_inorder f t),
   (=), (=),
   map_printer,
   [
     (Some("simple tree"),
      (identity,
       Node(l1, 2, l3)),
      Ok([1;2;3]));
     (* TODO *)
     (Some("empty tree"),
     (identity,
      Empty),
     Ok([]));

     (Some("single element"),
     (identity,
      Node(Empty, 1, Empty)),
     Ok([1]));

     (Some("half tree"),
     (identity,
      Node(Empty, 8, l1)),
     Ok([8;1]));

     (Some("lopsided tree"),
     (identity,
      Node(l6, 8, Empty)),
     Ok([0;6;1;8]));

     (Some("bigger tree"),
     (identity,
      Node(l6, 8, l7)),
     Ok([0;6;1;8;2;7;3]));
     
  ])


let map_revorder_tests =
  ("map_revorder",
   (fun (f,t) -> map_revorder f t),
   (=), (=),
   map_printer,
   [
     (Some("simple tree"),
      (identity,
       Node(l1, 2, l3)),
      Ok([3;2;1]));
     (* TODO *)
     (Some("empty tree"),
     (identity,
      Empty),
     Ok([]));

     (Some("single element"),
     (identity,
      Node(Empty, 1, Empty)),
     Ok([1]));

     (Some("half tree"),
     (identity,
      Node(Empty, 8, l1)),
     Ok([1;8]));

     (Some("lopsided tree"),
     (identity,
      Node(l6, 8, Empty)),
     Ok([8;1;6;0]));

     (Some("bigger tree"),
     (identity,
      Node(l6, 8, l7)),
     Ok([3;7;2;8;1;6;0]));

  ])

let is_bst_tests =
  ("is_bst",
   is_bst,
   (=), (=),
   Some(str_binary_tree,
        str_bool),
   [
     (Some("simple tree"),
      Node(Empty,
           1,
           Node(l2,
                3,
                Empty)),
      Ok(true));
     (* TODO *)
     (Some("simple incorrect tree"),
     Node(Empty,
          2,
          Node(l4,
               3,
               Empty)),
     Ok(false));
     
     (Some("empty tree"),
     Node(Empty,
          0,
          Empty),
     Ok(true));

     (Some("2 level tree"),
     Node(Node(l0,1,l2),
          3,
          Node(l4,
               5,
               Empty)),
     Ok(true));

     (Some("incorrect 2 level tree"),
     Node(Node(l0,1,l2),
          3,
          Node(l4,
               5,
               l3)),
     Ok(false));

     (Some("lopsided 2 level tree"),
     Node(Node(l0,1,l2),
          3,
          Empty),
     Ok(true));
  ])

let bst_max_tests =
  ("bst_max",
   bst_max,
   ((=) : data option -> data option -> bool),
   (=),
   Some(str_binary_tree,
        str_int_option),
   [
     (Some("simple tree"),
      Node(Empty,
           1,
           Node(l2, 3, Empty)),
      Ok(Some(3)));
     (* TODO *)
     (Some("balanced tree, single node"),
      Node(Empty,
           1,
           Empty),
      Ok(Some(1)));

      (Some("simple tree (right-leaning)"),
      Node(Empty,
           1,
           Node(Empty, 3, l4)),
      Ok(Some(4)));

      (Some("tree with duplicates"),
      Node(l5,
           5,
           l5),
      Ok(Some(5)));

      (Some("empty tree"),
      Empty,
      Ok(None));

      (Some("half tree"),
      Node(Empty,
           8,
           l1),
      Ok(Some(8)));
  ])


let bst_min_tests =
  ("bst_min",
   bst_min,
   ((=) : data option -> data option -> bool),
   (=),
   Some(str_binary_tree,
        str_int_option),
   [
     (Some("simple tree"),
      Node(Empty,
           1,
           Node(l2, 3, Empty)),
      Ok(Some(1)));
     (* TODO *)
     (Some("balanced tree, single node"),
      Node(Empty,
           1,
           Empty),
      Ok(Some(1)));

      (Some("simple tree (right-leaning)"),
      Node(Empty,
           1,
           Node(Empty, 3, l4)),
      Ok(Some(1)));

      (Some("tree with duplicates"),
      Node(l5,
           5,
           l5),
      Ok(Some(5)));

      (Some("empty tree"),
      Node(Empty,
           Empty,
           Empty),
      Error IndexError);

      (Some("half tree"),
      Node(Empty,
           8,
           l1),
      Ok(Some(1)));
  ])

let bst_insert_tests =
  ("bst_insert",
   (fun (t,x) ->  bst_insert t x),
   (=), (=),
   Some(tree_arg_printer, str_binary_tree),
   [
     (Some("simple tree"),
      (Node(l1, 2, l3),
       0),
      Ok (Node(Node(l0,1,Empty),
               2,
               l3)));
       (* TODO *)
     (Some("previously empty tree"),
      (Node(Empty, Empty, Empty),
       0),
      Ok (l0));

     (Some("right-leaning tree"),
      (Node(Empty, 2, l3),
       0),
      Ok (Node(0, 2, l3)));

     (Some("left-leaning tree"),
      (Node(l1, 2, Empty),
       3),
      Ok (Node(l1, 2, 3)));

     (Some("tree with duplicates"),
      (Node(l0, 1, l2),
       3),
      Ok (l0, 1, Node(Empty, 2, 3)));

     (Some("One Element at Start"),
      (Node(Empty, 2, Empty),
       3),
      Ok (Node(Empty, 2, 3)));
  ])


let bst_search_tests =
  ("bst_search",
   (fun (t,x) ->  bst_search t x),
   (=), (=),
   Some(tree_arg_printer, str_bool),
   [
     (Some("simple tree"),
      (Node(l1, 2, l3),
       2),
      Ok true);
     (* TODO *)
     (Some("empty tree"),
      (Node(Empty, Empty, Empty),
       2),
      Ok false);

     (Some("element not found"),
      (Node(l1, 2, l3),
       4),
      Ok false);

     (Some("duplicates"),
      (Node(2, 2, 2),
       2),
      Ok true);

     (Some("single element tree"),
      (Node(Empty, 8, Empty),
       7),
      Ok false);

     (Some("larger tree"),
      (Node(l6, 8, l7),
       7),
      Ok true);
   ])


let bst_remove_min_tests =
  ("bst_remove_min",
   bst_remove_min,
   (=), (=),
   Some(str_binary_tree,
        (fun (t,x) -> str_pair str_binary_tree str_int_option (t,x))),
   [
     (Some("simple tree"),
      Node(l1, 2, l3),
      Ok (Node(Empty,2,l3),
          Some 1));
     (Some("empty tree"),
      Empty,
      Ok (Empty, None));
     (* TODO *)
   ])


let bst_remove_tests =
  ("bst_remove",
   (fun(t,x) -> bst_remove t x),
   (=), (=),
   Some(tree_arg_printer,
        (fun (t,f) -> str_pair str_binary_tree str_bool (t,f))),
   [
     (Some("simple tree"),
      (Node(l1, 2, l3), 1),
      Ok ((Node (Empty,2,l3)), true));
     (* TODO *)
   ]) *)