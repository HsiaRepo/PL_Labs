open Util (* see util.ml *)

(******************)
(** Starter Code **)
(******************)

(*** Implementing higher-order functions ***)

let rec map (f : 'a->'b) (l : 'a list) : 'b list =
  (* TODO *)
  match l with
  | [] -> []
  | x :: xs -> f x :: map f xs

let rec filter (f : 'a->bool) (l : 'a list) : 'a list =
  (* TODO *)
  match l with
  | [] -> []
  | h::t -> if f h then h::(filter f t) else filter f t

let rec fold_left (f: 'y ->'x->'y) (y:'y) (l:'x list) : 'y =
  (* TODO *)
  match l with
  [] -> y
  | a::xs -> fold_left f (f y a) xs

let rec fold_right (f: 'y ->'x->'y) (y:'y) (l:'x list) : 'y =
  (* TODO *)
  match l with
  [] -> y
  | a::xs -> f a (fold_right f y xs)


(*** Using higher-order functions ***)


(* Concatenate two lists. *)
let append (l1 : 'a list) (l2 : 'a list) : 'a list =
  (* TODO *)
  fold_right (@) l2 [l1]



(* rev_append l1 l2 reverses l1 and concatenates it with l2*)
let rev_append (l1 : 'a list) (l2 : 'a list) : 'a list =
  (* TODO *)
  let l1_rev = fold_left (fun r x -> x::r) [] l1
in append l1_rev l2
  


(* Concatenate a list of lists. *)
let flatten (l : 'a list list) : 'a list =
  (* TODO *)
  fold_left (@) [] l



(* Insertion Sort *)

(* Insert elt into sorted list l in sorted order *)
let rec insert (cmp : 'a->'a->bool) (elt :'a) (l:'a list) : 'a list =
  (* TODO *)
  match l with
  | [] -> [elt]
  | h::t ->
    if cmp elt h
    then elt::h::t
    else h::(insert cmp elt t)


let insertionsort (cmp : 'a->'a->bool) (l:'a list) : 'a list =
  (* TODO *)
  fold_left (fun x y -> insert cmp y x) [] l
  (* fold_right cmp l [] *)



(* Selection Sort *)

(* Select the initial element from l based on cmp.  Return a tuple of
   the initial element and the list with the initial element
   removed. *)
let select (cmp : 'a->'a->bool) (l:'a list) : 'a * 'a list =
  match l with
  | [] -> invalid_arg "select"
  | a::d ->
     (* TODO *)
      fold_left (fun y x -> match y with
          |(elt, rst) -> if cmp x elt then (x, elt::rst) else (elt, x::rst)) (a, []) d

let rec selectionsort (cmp : 'a->'a->bool) (l:'a list) : 'a list =
  (* TODO *)
  match l with
  | [] -> []
  | a::d -> match select cmp l with
    | (a,d) -> a::(selectionsort cmp d)


(* Quicksort *)

(* Partion list l around elt.  Return a tuple consisting of all
   elements before elt and all elements after elt. *)
let pivot (cmp : 'a->'a->bool) (elt :'a) (l:'a list) : 'a list * 'a list =
  (* TODO *)
  fold_left (fun (left, right) x -> if cmp x elt then (x::left, right) else (left, x::right)) ([], []) l
  

(* The simple implementation of quicksort recurses on the two sublists
   and appends the sorted results. *)
let rec quicksort_simple (cmp : 'a->'a->bool) (l : 'a list) : 'a list =
  (* TODO *)
  match l with
  [] | _::[] -> l
  | p::tail -> 
      let left, right = pivot cmp p tail in
      quicksort_simple cmp left @ (p::quicksort_simple cmp right)
  

(* The better implementation of quicksort elides the append by passing
   a "tail" list to recursive calls.  Sorted results are directly
   cons'ed onto the tail, avoiding the need for an extra append. *)
   
let quicksort_better (cmp : 'a->'a->bool) (l : 'a list) : 'a list =
  let rec f (cmp : 'a->'a->bool) (l : 'a list) (r : 'a list) : 'a list =
    (* r is the tail: everything that must come after l in the sorted
       list. Passing r to f saves us from having to append sorted
       lists. *)
    (* TODO *)
    l @ r
  in f cmp l []
    

(***********)
(** Tests **)
(***********)

(* See description in testing.ml *)

let list_cmp cmp l1 l2 =
  (List.sort cmp l1) = (List.sort cmp l2)

let int_list_cmp l1 l2 =
  list_cmp (-) l1 l2


let map_tests =
  ("map", (fun (f,l)->map f l), (=), (=),
   Some((fun (f,l) -> str_int_list l),
        str_int_list),
   [
     (Some("simple list"), ((fun x -> 1+x), [1;2;3;4;5]), Ok [2;3;4;5;6]);
       (* TODO: Add more tests *)
     (Some("empty list"), ((fun x -> 1+x), []), Ok []);
     (Some("simple list with negatives"), ((fun x -> 2+x), [-1;-2;-3;-4;-5]), Ok [1;0;-1;-2;-3]);
     (Some("single value in list"), ((fun x -> x*4), [1000]), Ok [4000]);
     (Some("mixed values in list"), ((fun x -> 1+x), [-60;32;4;4;-50]), Ok [-59;33;5;5;-49]);
     (Some("multiple of the same value"), ((fun x -> 36/x), [18;18;18;18]), Ok [2;2;2;2]);
  ])

let filter_tests =
  ("filter", (fun (f,l)->filter f l), (=), (=),
   Some((fun (f,l) -> str_int_list l),
        str_int_list),
   [
     (Some("simple list"), ((fun x -> (x mod 2)=0), [1;2;3;4;5]), Ok [2;4]);
       (* TODO: Add more tests *)
     (Some("simple list with values less than certain value"), ((fun x -> x < 10), [-5;3;80;10;8]), Ok [-5;3;8]);
     (Some("empty list"), ((fun x -> (x mod 2)=0), []), Ok []);
     (Some("simple list (modulus and multiplication)"), ((fun x -> ((x*3) mod 2)=0), [5;10;15;20]), Ok [10;20]);
     (Some("simple list with simple comparison"), ((fun x -> x = 5), [1;2;3;4;5]), Ok [5]);
     (Some("simple list with zero value"), ((fun x -> x = 0), [0;1;2]), Ok [0]); 
  ])

let fold_left_tests =
  ("fold_left", (fun (f,y,l)->fold_left f y l), (=), (=),
   Some((fun (f,y,l) -> str_pair string_of_int str_int_list (y,l)),
        string_of_int),
   [
     (Some("+"), ((+), 0, [1;2;3]), Ok 6);
     (Some("-"), ((-), 0, [1;2;3]), Ok (-6));
       (* TODO: Add more tests *)
       (Some("*"), (( * ), 5, [5;10;25]), Ok 6250);
       (Some("/"), ((/), 8, [64;4;2]), Ok 0);
       (Some("mod"), ((mod), 3, [5;3;4]), Ok 0);
       (Some("empty"), (( * ), 1, []), Ok 1);
       (Some("0"), (( * ), 1, [0]), Ok 0);
  ])

let fold_right_tests =
  ("fold_right", (fun (f,y,l)->fold_right f y l), (=), (=),
   Some((fun (f,y,l) -> str_pair string_of_int str_int_list (y,l)),
        string_of_int),
   [
     (Some("+"), ((+), 0, [1;2;3]), Ok 6);
     (Some("-"), ((-), 0, [1;2;3]), Ok 2);
     (* TODO: Add more tests *)
     (Some("*"), (( * ), 5, [5;10;25]), Ok 6250);
     (Some("/"), ((/), 8, [2;4;16]), Ok 1);
     (Some("mod"), ((mod), 3, [4;3;5]), Ok 0);
     (Some("empty"), (( * ), 1, []), Ok 1);
     (Some("0"), (( * ), 1, [0]), Ok 0);


  ])


let append_tests =
  ("append", (fun (l1,l2)->append l1 l2), (=), (=),
   Some((fun x -> str_pair str_int_list  str_int_list x),
        str_int_list),
   [
     (Some("simple list"), ([1;2],[3;4]), Ok [1;2;3;4]);
       (* TODO: Add more tests *)
     (Some("multiple empty lists"), ([],[]), Ok []);
     (Some("simple list and empty list"), ([1;2;-3], []), Ok [1;2;-3]);
     (Some("simple list with repeated values"), ([1;1;3;9],[9;9;3;9;1]), Ok [1;1;3;9;9;9;3;9;1]);
     (Some("empty list and zero value list"), ([], [0]), Ok [0]);
     (Some("simple list with positive and negative values"), ([19;4;-55],[3]), Ok [19;4;-55;3]);
  ])

  
let rev_append_tests =
  ("rev_append", (fun (l1,l2)->rev_append l1 l2), (=), (=),
   Some((fun x -> str_pair str_int_list  str_int_list x),
        str_int_list),
   [
     (Some("simple list"), ([1;2],[3;4]), Ok [2;1;3;4]);
       (* TODO: Add more tests *)
     (Some("multiple empty lists"), ([],[]), Ok []);
     (Some("simple list and empty list"), ([1;2],[]), Ok [2;1]);
     (Some("empty list and simple list"), ([],[3;4]), Ok [3;4]);
     (Some("simple list with zeros"), ([0],[0]), Ok [0;0]);
     (Some("simple list with positive and negative values"), ([19;4;-55],[3]), Ok [-55;4;19;3]);
  ])

  
let flatten_tests =
  ("flatten", (fun l -> flatten l), (=), (=),
   Some((fun l -> "[" ^ str_x_list (str_int_list) l ";" ^ "]" ),
        str_int_list),
   [
     (Some("simple list"), [[1;2];[3;4]], Ok [1;2;3;4]);
     (Some("simple list 2"), [[3;4]; [1;2]], Ok [3;4;1;2]);
     (* TODO: Add more tests *)
     (Some("simple list with more than two lists"), [[1;2];[3;4];[5];[6;9]], Ok [1;2;3;4;5;6;9]);
     (Some("simple list with combination of empty and occupied lists"), [[1;2];[];[];[3;4];[];[5]], Ok [1;2;3;4;5]);
     (Some("empty list"), [], Ok []);
     (Some("empty list with empty lists within"), [[];[]], Ok []);
     (Some("simple list with positive and negative values"), [[19;4;-55];[3]], Ok [19;4;-55;3]);
   ]
  )


let sort_test_cases = [
    (Some("simple list"), ((<),[1;3;4;2;5]), Ok [1;2;3;4;5]);
    (* TODO: Add more tests *)
    (Some("empty list"), ((<),[]), Ok []);
    (Some("simple list (descending)"), ((>),[1;3;4;2;5]), Ok [5;4;3;2;1]);
    (Some("simple list with negative values (ascending)"), ((<),[-1;-3;-4;-2;-5]), Ok [-5;-4;-3;-2;-1]);
    (Some("simple list with negative and positive values (descending)"), ((>),[1;-3;4;-2;5]), Ok [5;4;1;-2;-3]);
    (Some("simple list with repeated values"), ((<),[1;0;0;0;1]), Ok [0;0;0;1;1]);
  ]


let insert_tests =
  ("insert", (fun (cmp,elt,l)->insert cmp elt l), (=), (=),
   Some(((fun (cmp,elt,l) -> str_pair string_of_int str_int_list (elt,l)),
         (fun y -> str_int_list y)
     )),
   [
     (Some("simple <"), ((<), 0, [-1;1;2]), Ok ([-1; 0; 1; 2]));
     (Some("simple >"), ((>), 0, [2;1;-1]), Ok ([2; 1; 0; -1]));
     (* TODO: Add more tests *)
     (None, ((<), 1, []), Ok ([1]));
     (None, ((<), 2, [1]), Ok ([1;2]));
     (None, ((>), 2, [1]), Ok ([2;1]));
     (None, ((<), 1, [1;2]), Ok ([1;1;2]));
     (None, ((>), 1, [2;1]), Ok ([2;1;1]));
   ])

let insertionsort_tests =
  ("insertionsort", (fun (cmp,l) -> insertionsort cmp l), (=), (=),
   Some((fun (cmp,l) -> str_int_list l),
        str_int_list),
   sort_test_cases)


let select_test_eq (s1,l1) (s2,l2) =
  (s1 = s2) && (int_list_cmp l1 l2)

let select_tests =
  ("select", (fun (cmp,l)->select cmp l), select_test_eq, (=),
   Some(((fun (cmp,l) -> str_int_list l),
         (fun (s,l) -> str_pair string_of_int str_int_list (s,l))
     )),
   [
     (Some("simple <"), ((<), [1;-1;2]), Ok (-1,[2;1]));
     (Some("simple >"), ((>), [1;-1;2]), Ok (2,[1;-1]));
     (* TODO: Add more tests *)
     (Some("simple < repeat"), ((<), [-1;2;-1;3]), Ok (-1,[2;-1;3]));
     (Some("simple > repeat"), ((>), [2;4;1;4]), Ok (4,[2;1;4]));
     (Some("simple < same"), ((<), [2;2;2]), Ok (2,[2;2]));
     (Some("simple > repeat"), ((>), [1;1;1]), Ok (1,[1;1]));


   ])


let selectionsort_tests =
  ("selectionsort", (fun (cmp,l) -> selectionsort cmp l), (=), (=),
   Some((fun (cmp,l) -> str_int_list l),
        str_int_list),
   sort_test_cases)


let pivot_test_eq (a1,b1) (a2,b2) =
  (int_list_cmp a1 a2) && (int_list_cmp b1 b2)

let pivot_tests =
  ("pivot", (fun (cmp,elt,l)->pivot cmp elt l), pivot_test_eq, (=),
   Some(((fun (cmp,elt,l) -> str_pair string_of_int str_int_list (elt,l)),
         (fun y -> str_pair str_int_list  str_int_list y)
     )),
   [
     (Some("simple <"), ((<), 0, [-1;1;0;-2; 2]), Ok ([-2; -1],[2; 0; 1]));
     (Some("simple >"), ((>), 0, [-1;1;0;-2; 2]), Ok ([2; 1], [-2; 0; -1]));
     (* TODO: Add more tests *)
     (Some("simple < with no pivot value in list"), ((<), 0, [-1;1;-2; 2]), Ok ([-2; -1],[2; 1]));
     (Some("simple > with no pivot value in list"), ((>), 0, [-1;1;-2; 2]), Ok ([2; 1],[-2; -1]));
     (Some("empty list"), ((>), 10, []), Ok ([], []));
     (Some("simple < with no values less than the pivot"), ((<), 0, [0;1;2]), Ok ([], [0;1;2]));
     (Some("simple > with no values more than the pivot"), ((>), 10, [-2;-1;0]), Ok ([-2;-1;0], []));
  ])


let quicksort_simple_tests =
  ("quicksort_simple", (fun (cmp,l) -> quicksort_simple cmp l), (=), (=),
   Some((fun (cmp,l) -> str_int_list l),
        str_int_list),
   sort_test_cases)
(*
let quicksort_better_tests =
  ("quicksort_simple", (fun (cmp,l) -> quicksort_better cmp l), (=), (=),
   Some((fun (cmp,l) -> str_int_list l),
        str_int_list),
   sort_test_cases)

   *)