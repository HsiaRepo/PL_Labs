
(* Functional helpers *)
let compose f g =
  fun x -> f (g x)

(* String Conversions *)

let str_bool x =
  if x then "true" else "false"

let str_float x =
  Printf.sprintf "%f" x

let rec str_pair (f : 'a -> string) (g : 'b -> string) ((a,b) : ('a * 'b)) : string =
   (f a)^""^
   (g b)

let rec str_option (f : 'a -> string) (o : 'a option) : string =
   match o with
   | None -> ""
   | Some(a) -> (f a)

let str_int_option (o:int option) : string =
  match o with
    None -> ""
  | Some(a) -> (string_of_int a)

let rec str_x_list (f : 'a -> string) (il : 'a list) (comma : string) : string =
  fst (List.fold_left
         (fun (str,flag) i ->
           (str^(if flag then "" else comma)^(f i), false))
         ("",true)
         il)


let str_int_list (l:int list) : string =
  "["^(str_x_list string_of_int l "; ")^"]"

(* Main helpers *)

let die_err (s : string) =
   output_string stderr s;
   output_string stderr "\n";
   exit 1

let open_input name =
  if "-" = name then stdin
  else open_in name
