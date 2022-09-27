open Javascript_ast
open Javascript_heap
open Javascript_main
open Util

(*
 * Check javascript_ast.ml for the following useful functionality:
 * - str_float               -- convert a float to a string
 * - to_num, to_bool, to_str -- do the JavaScript automatic type conversion
 * - read_environment        -- look up a variable's value in the environment
 * - push_environment        -- add a variable binding to the environment
 * - capture_environment     -- save the static (lexical) environment
 * - restore_environment     -- restore a static (lexical) environment
 * - empty_env               -- the empty environment
 *)

(*
 * Check javascript_heap.ml for the following useful functionality:
 * -  heap_alloc             -- allocate a new reference cell
 * -  heap_assign            -- update the value in a reference cell
 * -  heap_deref             -- retrieve the value from a reference
 *)


(* evaluate a program *)
let rec eval (env : environment_t) (h:heap_t) (p: program_t) : value_t*heap_t =
  match p with
  | ExprProgram(_,e) -> eval_expr env h e
  | StmtProgram(_,s,p) -> let (env1,_) = (eval_stmt env h s) in eval env1 h p

(* evaluate a block *)
and eval_block (env:environment_t) (h:heap_t) (p:block_t) : value_t*heap_t =
  match p with
  | ReturnBlock(_,e) -> eval_expr env h e
  | StmtBlock(_,s,b) -> let (env1,_) = (eval_stmt env h s) in eval_block env1 h b

(* evaluate a statement *)
and eval_stmt (env:environment_t) (h:heap_t) (s:stmt_t) : environment_t*heap_t =
  match s with
  (* TODO *)
  | ConstStmt(_,v,e) -> let (val1,heap1) = (eval_expr env h e) 
                        in (push_environment env v Immutable val1, heap1)

  | LetStmt(_,v,e) -> let (val1, heap1) = 
                        let (val2, heap2) = eval_expr env h e
                          in heap_alloc heap2 val2
                        in (push_environment env v Mutable val1, heap1)

  | AssignStmt(_,e1,e2) -> 
    match e1 with
    | VarExpr(_, x) -> 
      (match read_environment env x with
        | Some(Mutable, RefVal(ref_val)) -> 
              let (v, h1) = eval_expr env h e2
                in (env, heap_assign h1 ref_val v)

        | Some(Immutable, v) -> raise(ImmutableVar(x))
        | _ -> raise(UndeclaredVar(x)))
    | otherwise -> raise(RefError(e1))

  | _ -> raise (UnimplementedStmt(s))

(* evaluate a value *)
and eval_expr (env:environment_t) (h:heap_t) (e:expr_t) : value_t*heap_t =
  Printf.printf "%s %s\n" (str_expr e) (str_environment_simple env);
  match e with
  | BlockExpr(p,b) -> eval_block env h b
  | BopExpr(_,e1,AndBop,e2) -> (
    let v1,h = eval_expr env h e1 in
    if to_bool v1
    then eval_expr env h e2
    else v1, h
  )
  | ValExpr(_,v) -> v, h

  | VarExpr(p,v) -> (let x = read_environment env v in
    match x with
    | Some(_, RefVal(i)) -> (heap_deref h i), h
    | Some(_, n) -> n, h
    | otherwise -> raise(UndeclaredVar(v)), h
  )

  (* Evaluation of Unary expressions with a unary operator and an expression as parameters. 
     Returns corresponding boolean or numerical value. *)
  | UopExpr(_,uop,e) -> (match uop, eval_expr env h e with
    | PosUop, (NumVal(n), _) -> NumVal(n), h
    | NegUop, (NumVal(n), _) -> NumVal(-1.0 *. n), h
    | NegUop, (BoolVal(b), _) -> (match b with
      | true -> NumVal(-1.0), h
      | false -> NumVal(0.0), h
    )
    | NotUop, (BoolVal(b), _) -> BoolVal(not b), h
    | _ -> UndefVal, h
  )

  (* Evaluation of If expressions with three parameters. Returns corresponding boolean value. *)
  | IfExpr(_,e1,e2,e3) -> (match eval_expr env h e1 with
    | (BoolVal(true), _) -> (let val1 = eval_expr env h e2 in let val2 = eval_expr env h e3 in
      match val1, val2 with
        | (NumVal(x), _), (NumVal(_), _) -> NumVal(x), h

        | (BoolVal(x), _), (BoolVal(_), _) -> BoolVal(x), h

        | (StrVal(x), _), (StrVal(_), _) -> StrVal(x), h

        | otherwise -> UndefVal, h
    )  
    | (BoolVal(false), _) -> (let val1 = eval_expr env h e2 in let val2 = eval_expr env h e3 in
      match val1, val2 with
        | (NumVal(_), _), (NumVal(x), _) -> NumVal(x), h

        | (BoolVal(_), _), (BoolVal(x), _) -> BoolVal(x), h

        | (StrVal(_), _), (StrVal(x), _) -> StrVal(x), h
        | otherwise -> UndefVal, h
    )
  )
  (* Evaluation of Binary expressions with two expressions and a binary operator as parameters. 
     Binary operators include and, or, less than, less than or equal to, greater than, 
     greater than or equal to, equal to, and not equal to.
     Appropriate numeric or boolean values are returned. *)
  | BopExpr(_,e1,b,e2) -> (match b with
    | AndBop -> (let val1 = (eval_expr env h e1) in let val2 = (eval_expr env h e2) in
      (match val1, val2 with
        | (x, _), (y, _) -> BoolVal(to_bool x && to_bool y), h
        | _ -> UndefVal, h
      )
    )
    
    | OrBop -> (let val1 = (eval_expr env h e1) in let val2 = (eval_expr env h e2) in
      (match val1, val2 with
        | (NumVal(x), _), (y, _) -> NumVal(x), h
        | (x, _), (y, _) -> BoolVal(to_bool x || to_bool y), h
        | _ -> UndefVal, h
      )
    )
  
    | PlusBop -> (let val1 = (eval_expr env h e1) in let val2 = (eval_expr env h e2) in
      (match val1, val2 with
        | (StrVal(x), _), (y, _) -> StrVal(x ^ to_str y), h
        | (x, _), (StrVal(y), _) -> StrVal(to_str x ^ y), h
        | (x, _), (y, _) -> NumVal(to_num x +. to_num y), h
        | otherwise -> UndefVal, h
      )
    )

    | MinusBop -> (let val1 = (eval_expr env h e1) in let val2 = (eval_expr env h e2) in
      (match val1, val2 with
        | (x, _), (y, _) -> NumVal(to_num x -. to_num y), h
        | otherwise -> UndefVal, h
      )
    )

    | TimesBop -> (let val1 = (eval_expr env h e1) in let val2 = (eval_expr env h e2) in
      (match val1, val2 with
        | (x, _), (y, _) -> NumVal(to_num x *. to_num y), h
        | otherwise -> UndefVal, h
      )
    )

    | DivBop -> (let val1 = (eval_expr env h e1) in let val2 = (eval_expr env h e2) in
      (match val1, val2 with
        | (x, _), (y, _) -> NumVal(to_num x /. to_num y), h
        | otherwise -> UndefVal, h
      )
    )

    | LteBop -> (let val1 = (eval_expr env h e1) in let val2 = (eval_expr env h e2) in
      (match val1, val2 with
        | (x, _), (y, _) -> if (x >= y) then BoolVal(false), h else BoolVal(true), h
        | otherwise -> UndefVal, h
      )
    )

    | LtBop -> (let val1 = (eval_expr env h e1) in let val2 = (eval_expr env h e2) in
      (match val1, val2 with
        | (x, _), (y, _) -> if (x > y) then BoolVal(false), h else BoolVal(true), h
        | otherwise -> UndefVal, h
      )
    )

    | GtBop -> (let val1 = (eval_expr env h e1) in let val2 = (eval_expr env h e2) in
      (match val1, val2 with
        | (x, _), (y, _) -> if (x < y) then BoolVal(false), h else BoolVal(true), h
        | otherwise -> UndefVal, h
      )
    )

    | GteBop -> (let val1 = (eval_expr env h e1) in let val2 = (eval_expr env h e2) in
      (match val1, val2 with
        | (x, _), (y, _) -> if (x <= y) then BoolVal(false), h else BoolVal(true), h
        | otherwise -> UndefVal, h
      )
    )

    | NeqBop -> (let val1 = (eval_expr env h e1) in let val2 = (eval_expr env h e2) in
      (match val1, val2 with
        | (x, _), (y, _) -> BoolVal(x != y), h
        | otherwise -> UndefVal, h
      )
    )

    | EqBop -> (let val1 = (eval_expr env h e1) in let val2 = (eval_expr env h e2) in
      (match val1, val2 with
        | (x, _), (y, _) -> 
          (match x, y with
            | (x', y') -> BoolVal(x' = y'), h)
        | otherwise -> UndefVal, h
      )
    )
  )

  | PrintExpr(_,e) -> 
    (match eval_expr env h e with
      | (x, _) -> let _ = print_endline(to_str(x)) in UndefVal, h)

  | FuncExpr(_, l) -> (match l with
    | (Some(n), l, b, y) -> ClosureVal(env, (Some(n), l, b, y))
    | (None, l, b, y) -> ClosureVal(env, (None, l, b, y))), h

  | CallExpr(_, e1, expList) -> (match eval_expr env h e1 with
    | (ClosureVal (nmap, lambda), _) -> (let e2 = restore_environment env nmap in (match lambda with 
      | (v, vList, b, _) -> (let rec bind (env: environment_t) (vals: expr_t list) (nameList: typed_ident_t list) : environment_t =
        (match nameList, vals with
          | (env1, env2)::x1, env3::x2 -> 
              (match eval_expr env h env3 with 
                | (x, _) -> bind(push_environment env env1 Immutable x) x2 x1)
          | ([], []) -> env
        )
        in eval_block (bind e2 expList vList) h b)
      | _ -> UndefVal, h
    ))
    | _ -> UndefVal, h
  )

  | _ -> raise (UnimplementedExpr(e))

(*********)
(* Tests *)
(*********)

let test_group name tests =
  (name, compose (eval empty_env empty_heap) parse_string, eq_value_heap_value, eq_exn,
   Some((fun (x : string) -> x),
        (fun ((v,h) : value_t*heap_t) -> str_value v)),
   (* None, *)
   (List.map
      (fun (name,js,expected) ->
        (name,js,(match expected with
                  | Ok(v) -> Ok(v,empty_heap)
                  | Error(v) -> Error(v))))
     tests))

(* basic tests for the evaluator (do not modify) *)
let simple_expr_eval_tests =
  test_group "Simple Expression Evaluation"
    [
      (None, "1 + true",                     Ok(NumVal(2.0)));
      (None, "false + true",                 Ok(NumVal(1.0)));
      (None, "100 || 200",                   Ok(NumVal(100.0)));
      (None, "-false",                       Ok(NumVal(0.0)));
      (None, "1 + 1",                        Ok(NumVal(2.0)));
      (None, "3 + (4 + 5)",                  Ok(NumVal(12.0)));
      (None, "3 * (4 + 5)",                  Ok(NumVal(27.0)));
      (None, "-6 * 90 - 8",                  Ok(NumVal(-548.0)));
      (None, "-100 + 50",                    Ok(NumVal(-50.0)));
      (None, "true && (false || true)",      Ok(BoolVal(true)));
      (None, "true && (false || !true)",     Ok(BoolVal(false)));
      (None, "1 < 2",                        Ok(BoolVal(true)));
      (None, "100 === 100",                  Ok(BoolVal(true)));
      (None, "100 === 101",                  Ok(BoolVal(false)));
      (None, "100 !== 200",                  Ok(BoolVal(true)));
      (None, "true === true",                Ok(BoolVal(true)));
      (None, "0 / 0",                        Ok(NumVal(nan)));
      (None, "console.log(\"Hello World\")", Ok(UndefVal));
      (None, "(1 < 2) ? 123 : 124",          Ok(NumVal(123.0)));
      (None, "\"aaa\" < \"aaaa\"",           Ok(BoolVal(true)));
      (None, "\"bbb\" < \"aaa\"",            Ok(BoolVal(false)));
      (None, "\"hello\"+\" \"+\"world\"",    Ok(StrVal("hello world")));
      (None, "const x = 1; x+1",             Ok(NumVal(2.0)));
      (None, "const x=1; const y=2; x+y",    Ok(NumVal(3.0)));
      (None, "const x=3; const y=x*2+1; y",  Ok(NumVal(7.0)));
    ]

let fact_js = "function factorial(n){return (n <= 1) ? 1 : (n * factorial(n-1));}"
let fib_js = "function fib(x){return x<=0 ? 0 : (x===1 ? 1 : fib(x-1)+fib(x-2));}"
let scopes_js =
"(function (x) {
    return function(f) {
        return function (x) {
            return f(0);
        }(2);
    } (function (y) {return x;});
} (1))"

let readme1_js =
  "const f = function(x){ return x+1; };
   const r = f(2);
   r+3"

let readme2_js =
  "const x = 5;
   const f = function(y){ return x + y; };
   (function(z) { const x = 7; return f(6); })(0)"

(* basic tests for the evaluator (do not modify) *)
let simple_func_eval_tests =
  test_group "Simple Function Definition Evaluation"
    [
      (None, "function test(x){const x = 123; return x;}",
       Ok(ClosureVal(StringMap.empty,(
                Some("test"),
                [("x",None)],
                StmtBlock(NoPos,
                          ConstStmt(NoPos,
                                    "x",
                                    ValExpr(NoPos,NumVal(123.0))),
                          ReturnBlock(NoPos,VarExpr(NoPos,"x"))),
                None))));
      (None, fact_js,
       Ok(ClosureVal(StringMap.empty,(
                Some("factorial"),
                [("n",None)],
                ReturnBlock(NoPos,IfExpr(NoPos,
                                         BopExpr(NoPos,VarExpr(NoPos,"n"),LteBop,ValExpr(NoPos,NumVal(1.0))),
                                         ValExpr(NoPos,NumVal(1.0)),
                                         BopExpr(NoPos,VarExpr(NoPos,"n"),TimesBop,CallExpr(NoPos,VarExpr(NoPos,"factorial"),[BopExpr(NoPos,VarExpr(NoPos,"n"),MinusBop,ValExpr(NoPos,NumVal(1.0)))]))
                  )),
                None))));

    ]
(* note - you can use the following to print a program for debugging *)
(* let _ = Printf.printf "RESULT = %s\n" (str_program (parse_string "const x = 1 + 1; x * 2")) *)


let simple_call_eval_tests =
  test_group "Simple Call Evaluation"
    [
      (None, "const f = function(x){return x+1;}; f(1)",                    Ok(NumVal(2.0)));
      (None, "const y = 5; const f = function(x){return x+1;}; f(y)",       Ok(NumVal(6.0)));
      (Some("recursion"), "const f = function t(x){return x===0 ? 0 : x+t(x-1);}; f(5)", Ok(NumVal(15.0)));

      (Some("Readme 1"), readme1_js, Ok(NumVal(6.0)));
      (Some("Readme 2"), readme2_js, Ok(NumVal(11.0)));
      (Some("Lecture Scoping Test"), scopes_js, Ok(NumVal(1.0)))
    ]


(** Basic (instructor-provided) tests for mutability (do not modify.) *)
let simple_mut_eval_tests =
  test_group "Simple Mutability Evaluation"
    [
      (None, "1 = 100; 0",                   Error(RefError(ValExpr(NoPos,(NumVal(1.0))))));
      (None, "const x=3; x=1; x+1",          Error(ImmutableVar("x")));
      (None, "let x=3; x+1",                 Ok(NumVal(4.0)));
      (None, "let x=3; x=1; x+1",            Ok(NumVal(2.0)));
      (None, "let x=3; let y=4; x=x+y+5; y=10+y+x; x+y",            Ok(NumVal(38.0)));
      (None, "let x=3; x=true; x",            Ok(BoolVal(true)));
      (Some("static scope"), "let x=5; const f=function(y){return x+y;}; x=10; (function(z){const x=7; return f(6);})(0)", Ok(NumVal(16.0)));
]

let mut_eval_tests =
  test_group "Mutability Evaluation"
    [
      (* TODO *)
      (None, "1 = x; x",                   Error(RefError(ValExpr(NoPos,(NumVal(1.0))))));
      (None, "const x = 1;  let y = 1; y = x+y;",          Ok(NumVal(2.0)));
      (None, "let x = 1;  const y = 1; y = x+y;",          Error(ImmutableVar("y")));
      (None, "let x=3; x=true; let y = 4; y = x;",            Ok(BoolVal(true)));
      (Some("static scope"), "let x=1; const f=function(y){return x*y;}; x=2; (function(z){const x=3; return f(4);})(0)", Ok(NumVal(8.0)));
    ]
