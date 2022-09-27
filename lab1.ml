open Util (* see util.ml *)


(******************)
(** Starter Code **)
(******************)

exception IndexError

(* Return the i'th element of a list *)
let rec nth i l =
  (* TODO: *)
  match l with
    [] -> raise IndexError
  | a::d -> 
    if i = 0 then a
    else nth (i-1) d

(* Append two lists *)
let rec append l1 l2 =
  (* TODO *)
  match l1 with
    [] -> l2
  |  a::d -> a::append d l2

(* Reverse a list *)
let reverse = fun l ->
  (* TODO *)
  let rec h l acc =
    match l with
    | [] -> acc
    | a::d -> h d (a::acc)
    in h l []


(* Length of a list *)
let length l =
  (* TODO *)
  let rec h n l = 
    match l with
    | [] -> n
    | a::d -> h (n+1) d
  in h 0 l
  

(* Return the part of list l beginning at index 0 and ending at index
   iend *)
let rec list_prefix iend l =
  if iend < 0 then raise IndexError
  else if iend = 0 then []
  else
    match l with
      [] -> if iend = 0 then []
            else raise IndexError
    | a::d ->
       a :: list_prefix (iend-1) d

(* Return the part of list l beginning at istart and running through
   the end of the list *)
let rec list_suffix istart l =
  (* TODO *)
  if istart < 0 then raise IndexError
  else if istart = 0 then l
  else
    match l with
      [] -> if istart = 0 then []
            else raise IndexError
    | a::d -> list_suffix (istart-1) d

(* Merge sorted lists l1 and l2 based on cmp.
 *
 * When cmp returns true, its first argument should appear first in
   the merged lists.  *)


let rec merge cmp l1 l2 =
  (* append l1 l2 *)
  match (l1,l2) with 
  | ([],_) -> l2
  | (_,[]) -> l1
  | (a1::d1, a2::d2) -> 
    if cmp a1 a2 
    then a1::(merge cmp d1 l2)
      else a2::(merge cmp l1 d2)

let rec inTwo l l1 l2 = 
match l with
| [] -> (l1,l2)
| l::d -> 
  inTwo d l2 (l::l1)

(* Sort list l via mergesort *)
let rec mergesort cmp l = 
  match l with
  | [] -> l
  | _::[] -> l
  | _ -> let (l1,l2) = inTwo l [] [] 
      in (merge cmp (mergesort cmp l1) (mergesort cmp l2));;

(***********)
(** Tests **)
(***********)

(* See description in testing.ml *)

let nth_tests =
  ("Nth", (fun (i,l)->nth i l), (=), (=),
   Some((fun x -> str_pair string_of_int str_int_list x),
        string_of_int),
   [
     (Some("simple list"), (0, [1;2;3;4;5]), Ok 1);
     (Some("error: invalid index"), (-1, [1;2;3;4;5]), Error IndexError);
     (* TODO: Add more tests *)
     (Some("simple list (middle element)"), (1, [2;3;4]), Ok 3);
     (Some("simple list (last element)"), (2, [10;100;4]), Ok 4);
     (Some("error: invalid negative index"), (-2, [2;3;4]), Error IndexError);
     (Some("error: invalid positive index"), (1, [3]), Error IndexError);
     (Some("error: invalid index (empty list)"), (0, []), Error IndexError);
  ])

let append_tests =
  ("append", (fun (l1,l2)->append l1 l2), (=), (=),
   Some((fun x -> str_pair str_int_list  str_int_list x),
        str_int_list),
   [
     (Some("simple list"), ([1;2],[3;4]), Ok [1;2;3;4]);
       (* TODO: Add more tests *)
     (Some("simple list"), ([1], [999]), Ok [1;999]); 
     (Some("simple list (longer)"), ([2;81;67;2], [1000;-3;4;99999]), Ok [2;81;67;2;1000;-3;4;99999]); 
     (Some("simple list (append TO empty list)"), ([], [1;2;3;4]), Ok [1;2;3;4]);
     (Some("simple list (append empty list)"), ([1;2;3;4],[]), Ok [1;2;3;4]);
     (Some("simple list (two empty lists)"), ([],[]), Ok []);
  ])

let reverse_tests =
  ("reverse", reverse, (=), (=), Some(str_int_list,str_int_list),
   [
     (Some("simple list"), [1;2;3;4;5], Ok [5;4;3;2;1]);
       (* TODO: Add more tests *)
     (Some("simple short list"), [1;2], Ok [2;1]);  
     (Some("simple long list"), [5;4;3;2;1], Ok [1;2;3;4;5]);
     (Some("simple long list"), [18;68;44;45;47], Ok [47;45;44;68;18]);
     (Some("single-element list"), [1], Ok [1]);
     (Some("empty list"), [], Ok []);
  ])

let length_tests =
  ("length", length, (=), (=), Some(str_int_list,string_of_int),
   [
     (Some("simple list"), [1;2;3;4;5], Ok 5);
       (* TODO: Add more tests *)
     (Some("empty list"), [], Ok 0);
     (Some("simple list"), [1], Ok 1);
     (Some("simple list"), [1;2;3;4;5;5;4;3;2;1], Ok 10);
     (Some("simple list"), [4;9;4;2], Ok 4);
     (Some("simple list"), [99462118888;200;1], Ok 3);
  ])

let list_prefix_tests =
  ("list_prefix", (fun (iend,l) -> list_prefix iend l), (=), (=),
   Some((fun x -> str_pair string_of_int  str_int_list x),
        str_int_list),
   [
     (Some("simple list"), (2,[1;2;3;4;5]), Ok [1;2]);
     (* list_prefix is already implemented in the starter code *)
  ])

let list_suffix_tests =
  ("list_suffix", (fun (istart,l) -> list_suffix istart l), (=), (=),
   Some((fun x -> str_pair string_of_int  str_int_list x),
        str_int_list),
   [
     (Some("simple list"), (2,[1;2;3;4;5]), Ok [3;4;5]);
       (* TODO: Add more tests *)
     (Some("simple list: first element"), (1,[1;2;3;4;5]), Ok [2;3;4;5]);
     (Some("simple list: second to last element"), (4,[1;2;3;4;5]), Ok [5]);
     (Some("simple list: multiple of same element"), (2,[1;2;3;2;3;4;5]), Ok [3;2;3;4;5]);
     (Some("error: empty list"), (2,[]), Error IndexError);
     (Some("error: invalid index"), (6,[3;4;1;2]), Error IndexError);
  ])

  
let merge_tests =
  ("merge", (fun (cmp,l1,l2) -> merge cmp l1 l2), (=), (=),
   Some((fun (cmp,l1,l2) -> str_pair str_int_list str_int_list (l1, l2)),
        str_int_list),
   [
     (Some("simple list"), ((<),[1;3],[2;4;5]), Ok [1;2;3;4;5]);
       (* TODO: Add more tests *)
     (Some("simple list"), ((<),[1;3;5],[2;4]), Ok [1;2;3;4;5]);
     (Some("simple list (equal number of elements)"), ((<),[1;3;5],[2;4;6]), Ok [1;2;3;4;5;6]);
     (Some("empty lists"), ((<),[],[]), Ok []);
     (Some("empty list on left"), ((<),[],[1]), Ok [1]);
     (Some("empty list on right"), ((<),[2],[]), Ok [2]);
  ])


let mergesort_tests =
  ("mergesort", (fun (cmp,l) -> mergesort cmp l), (=), (=),
   Some((fun (cmp,l) -> str_int_list l),
        str_int_list),
   [
     (Some("simple list"), ((<),[1;3;4;2;5]), Ok [1;2;3;4;5]);
     (* TODO: Add more tests *)
     (Some("simple list sort"), ((<),[1;2;3;4;5]), Ok [1;2;3;4;5]);
     (Some("descending"), ((>),[5;3;4;2;1]), Ok [5;4;3;2;1]);
     (Some("empty list"), ((<),[]), Ok []);
     (Some("different numbers"), ((<),[4;2;0;6;9]), Ok [0;2;4;6;9]);
     (Some("long sort"), ((<),[9;8;7;5;6;3;2;4;1]), Ok [1;2;3;4;5;6;7;8;9]);
   ])
