open Lexing;;
open Parsing;;
open Util;;

(* open Flags;; *)
let eq_base (a : 'a) (b : 'a) : bool = (a = b) ;;
let rec eq_option (f : 'a -> 'a -> bool) (a : 'a option) (b : 'a option) : bool =
   match (a,b) with
   | (None,None) -> true
   | (Some(a),Some(b)) -> (f a b)
   | _ -> false
;;
let eq_pair (f1 : 'a -> 'a -> bool) (f2 : 'b -> 'b -> bool) ((p1a,p1b) : 'a * 'b) ((p2a,p2b) : 'a * 'b) : bool = ((f1 p1a p2a) && (f2 p1b p2b)) ;;
let eq_list (f : 'a -> 'a -> bool) (l1 : 'a list) (l2 : 'a list) : bool = try List.fold_left2 (fun res l1i l2i -> res && (f l1i l2i)) true l1 l2 with _ -> false;;


module StringSet = Set.Make(
  struct
    type t = string
    let compare = compare
  end)

module IntMap = Map.Make(
  struct
    type t = int
    let compare = compare
  end)

module StringMap = Map.Make(
  struct
    type t = string
    let compare = compare
  end)

let my_unescaped s =
  let s = Str.global_replace (Str.regexp "[\\][\r][\n]") "" s in
  let s2 = Str.global_replace (Str.regexp "[\\][\n]") "" s in
  Scanf.unescaped s2

let strip_quotes (s : string) : (string*bool) =
  let len = String.length s in
  if ((len >= 2) && (String.get s 0)='"' && (String.get s (len-1))='"')
  then (String.sub s 1 (len-2),true) else (s,false)

(* AST Data Structure *)

type start_t =
   | JavascriptProgram of pos_t * program_t

 and program_t =
   | ExprProgram of pos_t * expr_t
   | StmtProgram of pos_t * stmt_t * program_t

 and stmt_t =
   | ConstStmt of pos_t * ident_t * expr_t
   | LetStmt of pos_t * ident_t * expr_t
   | AssignStmt of pos_t * expr_t * expr_t

 and typ_t =
   | UnitType
   | BoolType
   | NumType
   | StrType
   | TupleType of typ_t list
   | FuncType of typ_t * typ_t

 and var_access_t =
   | Mutable
   | Immutable

 and uop_t = (* unary operation *)
   | NotUop
   | NegUop
   | PosUop

 and bop_t = (* binary operation *)
   | AndBop
   | OrBop
   | PlusBop
   | MinusBop
   | TimesBop
   | DivBop
   | EqBop
   | NeqBop
   | LtBop
   | LteBop
   | GtBop
   | GteBop

 and expr_t =
   | VarExpr of pos_t * ident_t
   | ValExpr of pos_t * value_t
   | BlockExpr of pos_t * block_t
   | FuncExpr of pos_t * lambda_t
   | UopExpr of pos_t * uop_t * expr_t
   | BopExpr of pos_t * expr_t * bop_t * expr_t
   | IfExpr of pos_t * expr_t * expr_t * expr_t
   | PrintExpr of pos_t * expr_t
   | CallExpr of pos_t * expr_t * expr_t list
   | FieldExpr of pos_t * expr_t * ident_t
   | ObjectExpr of pos_t * field_t list

 and value_t =
   | NumVal of float
   | BoolVal of bool
   | StrVal of string
   | UndefVal
   | ClosureVal of name_map_t * lambda_t
   | RefVal of address_t

 and ident_t = string

 and address_t = int

 and typed_ident_t = (ident_t * typ_t option)

 and name_map_t = (var_access_t * value_t) StringMap.t

 and environment_t = name_map_t

 and lambda_t = (ident_t option * typed_ident_t list * block_t * typ_t option)


 and object_t = (value_t StringMap.t)

 and block_t =
   | ReturnBlock of pos_t * expr_t
   | StmtBlock of pos_t * stmt_t * block_t

 and field_list_t = (field_t list)

 and expr_list_t = (expr_t list)


 and ident_list_t = (ident_t list)


 and field_t = (ident_t * expr_t)

 and bool_t = bool

(* data type for file positions *)
and pos_t = NoPos | Pos of string*int*int (* file,line,col *)

let filename = ref ""

exception Parse_error of string;;
exception Lexing_error of string;;
exception General_error of string;;

(* do_error p s
 *
 * Print error message
 *
 * p - the location of the error
 * s - the error message
 *
 * returns unit
 *)
let do_error (p : pos_t) (s : string) : string =
   ("Error"^
   (match p with
   | NoPos -> ""
   | Pos(file_name,line_num,col_num) -> (" in '"^file_name^
    "' on line "^(string_of_int line_num)^" col "^(string_of_int
    col_num))
   )^
   (": "^s^"\n"))
;;

let die_error (p : pos_t) (s : string) =
   raise (General_error(do_error p s))
;;

(* gets a pos data structure using the current lexing pos *)
let get_current_pos () =
   let p         = symbol_start_pos () in
   let file_name = !filename (*p.Lexing.pos_fname*)  in
   let line_num  = p.Lexing.pos_lnum   in
   let col_num   = (p.Lexing.pos_cnum-p.Lexing.pos_bol+1) in
   Pos(file_name,line_num,col_num)
;;

(* gets a pos data structure from a lexing position *)
let get_pos (p : Lexing.position) =
   let file_name = !filename (*p.Lexing.pos_fname*) in
   let line_num  = p.Lexing.pos_lnum  in
   let col_num   = (p.Lexing.pos_cnum-p.Lexing.pos_bol+1) in
   Pos(file_name,line_num,col_num)
;;

(* dies with a general position-based error *)
let pos_error (s : string) (p : position) =
   do_error (get_pos p) s
;;

(* dies with a parse error s *)
let parse_error (s : string) =
   raise (Parse_error(pos_error s (symbol_end_pos ())))
;;

(* dies with a lexing error *)
let lex_error (s : string) (lexbuf : Lexing.lexbuf) =
   raise (Lexing_error(pos_error s (Lexing.lexeme_end_p lexbuf)))
;;

(* updates the lexer position to the next line
 * (this is used since we skip newlines in the
 *  lexer, but we still wish to remember them
 *  for proper line positions) *)
let do_newline lb =
   Lexing.new_line lb
;;

(* dies with a system error s *)
let die_system_error (s : string) =
   output_string stderr s;
   output_string stderr "\n";
   exit 1
;;

let rec count_newlines s lb = match s with
  | "" -> 0
  | _  -> let c = String.sub s 0 1 in
          let cs = String.sub s 1 ((String.length s)-1) in
          if (c="\n") then (do_newline lb; 1+(count_newlines cs lb))
                      else (count_newlines cs lb)
;;



(* obtain most recent binding of variable x in environment env *)
let read_environment (env:environment_t) (x:string)
    : (var_access_t*value_t) option =
  try Some(StringMap.find x env)
  with _ -> None

(* push a new binding for variable x into the environment env *)
let push_environment (env:environment_t) (x:string) (access:var_access_t) (value:value_t)
    : environment_t =
  StringMap.add x (access,value) env


(* TODO: remove these function *)
let capture_environment (env:environment_t) : name_map_t  = env

let restore_environment (env:environment_t) (captured:name_map_t) : environment_t  =
  captured

(* empty environment - used for testing *)
let empty_env = StringMap.empty

(* floating-point equality - used only for testing functionality *)
let eq_float = fun (n1,n2) -> (compare n1 n2)=0

(* convert JavaScript numeric value to float *)
let js_float_of_string s =
  try float_of_string s
  with _ -> (
    try Int64.to_float (Int64.of_string s)
    with _ -> raise (Failure(Printf.sprintf "js_float_of_string: %s" s))
  )


(* convert float to string *)
let str_float n =
  if (eq_float (n,nan)) then "NaN"
  else if (eq_float (n,infinity)) then "Infinity"
  else if (eq_float (n,neg_infinity)) then "-Infinity"
  else
    let s = Printf.sprintf "%g" n in
    let len1 = (String.length s)-1 in
    if (String.get s len1)='.'
    then String.sub s 0 len1
    else s

let rec str_typ (t : typ_t) = match t with
  | UnitType -> "undefined"
  | BoolType -> "boolean"
  | NumType -> "number"
  | StrType -> "string"
  | TupleType(tl) -> Printf.sprintf "(%s)" (str_x_list str_typ tl ", ")
  | FuncType(t1,t2) -> Printf.sprintf "%s -> %s" (str_typ t1) (str_typ t2)

let str_access a = match a with
  | Mutable   -> "mut"
  | Immutable -> "const"

let rec str_program (p : program_t) = match p with
  | ExprProgram(_,e) -> str_expr e
  | StmtProgram(_,s,p) -> Printf.sprintf "%s; %s" (str_stmt s) (str_program p)

and str_stmt (s : stmt_t) = match s with
  | ConstStmt(_,v,e) -> Printf.sprintf "const %s = %s" (str_ident v) (str_expr e)
  | LetStmt(_,v,e) -> Printf.sprintf "let %s = %s" (str_ident v) (str_expr e)
  | AssignStmt(_,e1,e2) -> Printf.sprintf "%s = %s" (str_expr e1) (str_expr e2)

and str_expr (e : expr_t) = match e with
  | VarExpr(_,v) -> Printf.sprintf "%s" (str_ident v)
  | ValExpr(_,v) -> Printf.sprintf "%s" (str_value v)
  | BlockExpr(_,b) -> Printf.sprintf "%s" (str_block b)
  | FuncExpr(_,f) -> Printf.sprintf "%s" (str_lambda f)
  | UopExpr(_,uop,e) -> Printf.sprintf "(%s %s)" (str_uop uop) (str_expr e)
  | BopExpr(_,e1,bop,e2) -> Printf.sprintf "(%s %s %s)" (str_expr e1) (str_bop bop) (str_expr e2)
  | CallExpr(_,(VarExpr(_) as e1),e2) -> Printf.sprintf "%s(%s)" (str_expr e1) (str_x_list str_expr e2 ", ")
  | CallExpr(_,e1,e2) -> Printf.sprintf "(%s)(%s)" (str_expr e1) (str_x_list str_expr e2 ", ")
  | IfExpr(_,e1,e2,e3) -> Printf.sprintf "(%s ? %s : %s)" (str_expr e1) (str_expr e2) (str_expr e3)
  | PrintExpr(_,e) -> Printf.sprintf "console.log(%s)" (str_expr e)
  | FieldExpr(_,e,f) -> Printf.sprintf "%s.%s" (str_expr e) (str_ident f)
  | ObjectExpr(_,fl) -> Printf.sprintf "{ %s }" (str_x_list (fun (f,e) -> Printf.sprintf "%s:%s" (str_ident f) (str_expr e)) fl ", ")

and str_bop (b : bop_t) = match b with
  | AndBop -> "&&"
  | OrBop -> "||"
  | PlusBop -> "+"
  | MinusBop -> "-"
  | TimesBop -> "*"
  | DivBop -> "/"
  | EqBop -> "==="
  | NeqBop -> "!=="
  | LtBop -> "<"
  | LteBop -> "<="
  | GtBop -> ">"
  | GteBop -> ">="

and str_uop (u : uop_t) = match u with
  | NotUop -> "!"
  | NegUop -> "-"
  | PosUop -> "+"

and str_value (v : value_t) = str_value_helper v false

and str_value_helper (v : value_t) (print_special : bool) = match v with
  | NumVal(n) -> str_float n
  | BoolVal(b) -> string_of_bool b
  | StrVal(s) -> Printf.sprintf "\"%s\"" s
  | UndefVal -> "undefined"
  | ClosureVal(nm,f) ->
    if print_special then Printf.sprintf "CL<%s; %s>" (str_name_map nm) (str_lambda f)
    else str_lambda f
  | RefVal(addr) -> Printf.sprintf "REF<%d>" addr

and str_lambda (name,params,bl,_) =
  Printf.sprintf "function %s(%s) { %s }" (str_option (fun x -> x) name) (str_x_list str_typed_var params ", ") (str_block bl)

and str_block (bl : block_t) = match bl with
  | ReturnBlock(_,e) -> Printf.sprintf "return %s" (str_expr e)
  | StmtBlock(_,s,bl) -> Printf.sprintf "%s; %s" (str_stmt s) (str_block bl)

and str_typed_var (tv : typed_ident_t) = match tv with
  | (v,t) -> v

and str_ident (x : ident_t) = x

and str_name_map nm =
  (* TODO: combine with str_environment *)
  "{" ^
    (str_x_list
       (fun (k,(m,v)) ->
         Printf.sprintf "%s->(%s,%s)" k (str_access m) (str_value_helper v true))
       (StringMap.bindings nm) ", ")
    ^ "}"

let str_ast a = match a with
  | JavascriptProgram(_, x) -> str_program x

let rec eq_typ (t1 : typ_t) (t2 : typ_t) = match (t1,t2) with
  | (UnitType,UnitType) -> true
  | (BoolType,BoolType) -> true
  | (NumType,NumType) -> true
  | (StrType,StrType) -> true
  | (TupleType(tl1),TupleType(tl2)) -> eq_list eq_typ tl1 tl2
  | (FuncType(ta1,tb1),FuncType(ta2,tb2)) -> (eq_typ ta1 ta2) && (eq_typ tb1 tb2)
  | _ -> false

let rec eq_program (p1 : program_t) (p2 : program_t) = match (p1,p2) with
  | (ExprProgram(_,e1),ExprProgram(_,e2)) -> eq_expr e1 e2
  | (StmtProgram(_,s1,p1),StmtProgram(_,s2,p2)) -> (eq_stmt s1 s2) && (eq_program p1 p2)
  | _ -> false

and eq_stmt (s1 : stmt_t) (s2 : stmt_t) = match (s1,s2) with
  | (ConstStmt(_,v1,e1),ConstStmt(_,v2,e2)) -> (eq_ident v1 v2) && (eq_expr e1 e2)
  | (LetStmt(_,v1,e1),LetStmt(_,v2,e2)) -> (eq_ident v1 v2) && (eq_expr e1 e2)
  | (AssignStmt(_,ea1,eb1),AssignStmt(_,ea2,eb2)) -> (eq_expr ea1 ea2) && (eq_expr eb1 eb2)
  | _ -> false

and eq_expr (e1 : expr_t) (e2 : expr_t) = match (e1,e2) with
  | (VarExpr(_,v1),VarExpr(_,v2)) -> eq_ident v1 v2
  | (ValExpr(_,v1),ValExpr(_,v2)) -> eq_value v1 v2
  | (BlockExpr(_,b1),BlockExpr(_,b2)) -> eq_block b1 b2
  | (FuncExpr(_,f1),FuncExpr(_,f2)) -> eq_lambda f1 f2
  | (UopExpr(_,uop1,e1),UopExpr(_,uop2,e2)) -> (eq_uop uop1 uop2) && (eq_expr e1 e2)
  | (BopExpr(_,ea1,bop1,eb1),BopExpr(_,ea2,bop2,eb2)) -> (eq_expr ea1 ea2) && (bop1 = bop2) && (eq_expr eb1 eb2)
  | (CallExpr(_,ea1,eb1),CallExpr(_,ea2,eb2)) -> (eq_expr ea1 ea2) && (eq_list eq_expr eb1 eb2)
  | (IfExpr(_,ea1,eb1,ec1),IfExpr(_,ea2,eb2,ec2)) -> (eq_expr ea1 ea2) && (eq_expr eb1 eb2) && (eq_expr ec1 ec2)
  | (PrintExpr(_,e1),PrintExpr(_,e2)) -> eq_expr e1 e2
  | (FieldExpr(_,e1,f1),FieldExpr(_,e2,f2)) -> (eq_expr e1 e2) && (eq_ident f1 f2)
  | (ObjectExpr(_,fl1),ObjectExpr(_,fl2)) -> eq_list (fun (i1,e1) (i2,e2) -> (eq_ident i1 i2) && (eq_expr e1 e2)) fl1 fl2
  | _ -> false


and eq_uop (u : uop_t) (u : uop_t) = match (u,u) with
  | (NotUop,NotUop) -> true
  | (NegUop,NegUop) -> true
  | (PosUop,PosUop) -> true
  | _ -> false

and eq_value (v1 : value_t) (v2 : value_t) = match (v1,v2) with
  | (NumVal(n1),NumVal(n2)) -> eq_float (n1,n2)
  | (BoolVal(b1),BoolVal(b2)) -> b1=b2
  | (StrVal(s1),StrVal(s2)) -> s1=s2
  | (UndefVal,UndefVal) -> true
  | (ClosureVal(nm1,l1),ClosureVal(nm2,l2)) ->
    StringMap.equal (=) nm1 nm2 && eq_lambda l1 l2
  | (RefVal(a1),RefVal(a2)) -> a1=a2
  | _ -> false


and eq_lambda (name1,params1,bl1,t1) (name2,params2,bl2,t2) =
  (eq_option eq_ident name1 name2) && (eq_list eq_typed_var params1 params2)
  && (eq_block bl1 bl2) && (eq_option eq_typ t1 t2)

and eq_block (bl1 : block_t) (bl2 : block_t) = match (bl1,bl2) with
  | (ReturnBlock(_,e1),ReturnBlock(_,e2)) -> eq_expr e1 e2
  | (StmtBlock(_,s1,bl1),StmtBlock(_,s2,bl2)) -> (eq_stmt s1 s2) && (eq_block bl1 bl2)
  | _ -> false

and eq_typed_var (tv1 : typed_ident_t) (tv2 : typed_ident_t) = match (tv1,tv2) with
  | ((v1,t1),(v2,t2)) -> (eq_ident v1 v2) && (eq_option eq_typ t1 t2)

and eq_ident (x1 : ident_t) (x2 : ident_t) =
  (x1=x2)


(* convert value to float *)
let to_num (v : value_t) =
  match v with
  | NumVal(n) -> n
  | BoolVal(true) -> 1.0
  | BoolVal(false) -> 0.0
  | StrVal(s) ->
     if (String.trim s)="" then 0.0
     else (try js_float_of_string s with _ -> nan)
  | UndefVal -> nan
  | ClosureVal(_,_) -> nan
  | RefVal(_) -> nan

(* convert value to bool *)
let to_bool (v : value_t) =
  match v with
  | NumVal(n) -> if (eq_float (n,0.0) || eq_float (n,nan)) then false else true
  | BoolVal(b) -> b
  | StrVal("") -> false
  | StrVal(_) -> true
  | UndefVal -> false
  | ClosureVal(_,_) -> true
  | RefVal(_) -> true

(* convert value to string *)
let to_str (v : value_t) =
  match v with
  | NumVal(n) -> str_float n
  | BoolVal(b) -> string_of_bool b
  | StrVal(s) -> s
  | UndefVal -> "undefined"
  | ClosureVal(_,_) -> str_value v
  | RefVal(_) -> "[object Object]"

let rec str_environment (e:environment_t) =
  str_name_map e

and str_environment_simple (e:environment_t) : string =
  str_environment e

and str_object (o:object_t) =
  "{"^(str_x_list (fun (k,v) -> Printf.sprintf "%s->%s" k (str_value v)) (StringMap.bindings o) ", ")^"}"

(* exception indicating that a program is more
 * than just a single expression *)
exception NotExpr of program_t
(* exception indicating unimplemented input
 * expression *)
exception ImmutableVar of ident_t
exception UndeclaredVar of ident_t
exception InvalidCall of expr_t
exception UnimplementedExpr of expr_t
exception UnimplementedStmt of stmt_t
exception UnimplementedBlock of block_t
exception UnimplementedProgram of program_t
exception RefError of expr_t

let eq_exn ex1 ex2 =
  match (ex1,ex2) with
  | (NotExpr(p1),NotExpr(p2)) -> eq_program p1 p2
  | (ImmutableVar(i1),ImmutableVar(i2)) -> i1=i2
  | (UndeclaredVar(i1),UndeclaredVar(i2)) -> i1=i2
  | (InvalidCall(e1),InvalidCall(e2)) -> eq_expr e1 e2
  | (UnimplementedExpr(e1),UnimplementedExpr(e2)) -> eq_expr e1 e2
  | (UnimplementedStmt(s1),UnimplementedStmt(s2)) -> eq_stmt s1 s2
  | (RefError(e1),RefError(e2)) -> eq_expr e1 e2
  | (ex1,ex2) -> ex1=ex2

(* add printer for above exceptions *)
let _ = Printexc.register_printer (fun ex ->
            match ex with
            | NotExpr(p) -> Some(Printf.sprintf "NotExpr(%s)" (str_program p))
            | ImmutableVar(i) -> Some(Printf.sprintf "ImmutableVar(%s)" i)
            | UndeclaredVar(i) -> Some(Printf.sprintf "UndeclaredVar(%s)" i)
            | InvalidCall(e) -> Some(Printf.sprintf "InvalidCall(%s)" (str_expr e))
            | UnimplementedExpr(e) -> Some(Printf.sprintf "Unimplemented(%s)" (str_expr e))
            | UnimplementedStmt(s) -> Some(Printf.sprintf "Unimplemented(%s)" (str_stmt s))
            | RefError(e) -> Some(Printf.sprintf "RefError(%s)" (str_expr e))
            | _ -> None)