open Javascript_ast
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
 * (eval env p) should reduce a program in initial environment env to a *value*
 * In general, if Node.js produces a *value* for an example JavaScript
 * program, your evaluator should produce that same value.
 * You should support basic JavaScript (recursive higher-order) functions
 * with lexical scoping.
 *
 * See the assignment writeup for more details.
 *)


(* evaluate a program *)
let rec eval  (env : environment_t) (p: program_t) : value_t = match p with
  | ExprProgram(_,e) -> eval_expr env e
  | StmtProgram(_,s,p) -> eval (eval_stmt env s) p

(* evaluate a block *)
and eval_block  (env:environment_t) (p:block_t) : value_t = match p with
  | ReturnBlock(_,e) -> eval_expr env e
  | StmtBlock(_, e, b) -> eval_block env b

(* evaluate a statement *)
and eval_stmt (env:environment_t) (s:stmt_t) : environment_t = match s with
  | ConstStmt(_,v,e) -> push_environment env v Immutable (eval_expr env e)
  (* Don't need these till Lab9
  | LetStmt(_,v,e) -> push_environment env v Immutable (eval_expr env v)
  | AssignStmt(_,e1,e2) -> push_environment env e1 Mutable (eval_expr env e1)
  | _ -> raise (UnimplementedStmt(s))
  *)

(* evaluate an expression *)
and eval_expr (env:environment_t) (e:expr_t) : value_t =
  match e with
  | BlockExpr(p,b) -> eval_block env b
  | BopExpr(_,e1,AndBop,e2) -> (
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    if (to_bool v1) then v2 else v1
  )
  
  | ValExpr(_,v) -> v

  | VarExpr(p,v) -> (let x = read_environment env v in
    match x with
    | Some(_, n) -> n
    | otherwise -> UndefVal
  )

  (* Evaluation of Unary expressions with a unary operator and an expression as parameters. 
     Returns corresponding boolean or numerical value. *)
  | UopExpr(_,uop,e) -> (match uop, eval_expr env e with
    | PosUop, NumVal(n) -> NumVal(n)
    | NegUop, NumVal(n) -> NumVal(-1.0 *. n)
    | NegUop, BoolVal(b) -> (match b with
      | true -> NumVal(-1.0)
      | false -> NumVal(0.0)
    )
    | NotUop, BoolVal(b) -> BoolVal(not b)
    | _ -> UndefVal
  )

  (* Evaluation of If expressions with three parameters. Returns corresponding boolean value. *)
  | IfExpr(_,e1,e2,e3) -> (match eval_expr env e1 with
    | BoolVal(true) -> (let val1 = eval_expr env e2 in let val2 = eval_expr env e3 in
      match val1, val2 with
        | NumVal(_), NumVal(_) -> NumVal(to_num val1)
        | BoolVal(_), BoolVal(_) -> BoolVal(to_bool val1)
        | StrVal(_), StrVal(_) -> StrVal(to_str val1)
        | otherwise -> UndefVal
    )  
    | BoolVal(false) -> (let val1 = eval_expr env e2 in let val2 = eval_expr env e3 in
      match val1, val2 with
        | NumVal(_), NumVal(_) -> NumVal(to_num val2)
        | BoolVal(_), BoolVal(_) -> BoolVal(to_bool val2)
        | StrVal(_), StrVal(_) -> StrVal(to_str val2)
        | otherwise -> UndefVal
    )
  )
  (* Evaluation of Binary expressions with two expressions and a binary operator as parameters. 
     Binary operators include and, or, less than, less than or equal to, greater than, 
     greater than or equal to, equal to, and not equal to.
     Appropriate numeric or boolean values are returned. *)
  | BopExpr(_,e1,b,e2) -> (match b with
    | AndBop -> (let val1 = (eval_expr env e1) in let val2 = (eval_expr env e2) in
      (match val1, val2 with
        | x, y -> BoolVal(to_bool x && to_bool y)
        | _ -> UndefVal
      )
    )
    
    | OrBop -> (let val1 = (eval_expr env e1) in let val2 = (eval_expr env e2) in
      (match val1, val2 with
        | BoolVal(_), BoolVal(_) -> BoolVal(to_bool val1 || to_bool val2)
        | NumVal(_), NumVal(_) -> (match to_bool val1, to_bool val2 with
          | true, true -> NumVal(to_num val1)
          | true, false -> NumVal(to_num val1)
          | false, true -> NumVal(to_num val2)
          | false, false -> NumVal(to_num val2)
        )
        | _ -> UndefVal
      )
    )
  
    | PlusBop -> (let val1 = (eval_expr env e1) in let val2 = (eval_expr env e2) in
      (match val1, val2 with
        | NumVal(_), NumVal(_) -> NumVal(to_num val1 +. to_num val2)
        | NumVal(_), BoolVal(_) -> NumVal(to_num val1 +. to_num val2)
        | BoolVal(_), BoolVal(_) -> NumVal(to_num val1 +. to_num val2)
        | BoolVal(_), NumVal(_) -> NumVal(to_num val1 +. to_num val2)
        | StrVal(_), StrVal(_) -> StrVal(to_str val1 ^ to_str val2)
        | StrVal(_), (_) -> StrVal(to_str val1 ^ to_str val2)
        | (_), StrVal(_) -> StrVal(to_str val1 ^ to_str val2)
        | otherwise -> UndefVal
      )
    )

    | MinusBop -> (let val1 = (eval_expr env e1) in let val2 = (eval_expr env e2) in
      (match val1, val2 with
        | NumVal(_), NumVal(_) -> NumVal(to_num val1 -. to_num val2)
        | NumVal(_), BoolVal(_) -> NumVal(to_num val1 -. to_num val2)
        | BoolVal(_), BoolVal(_) -> NumVal(to_num val1 -. to_num val2)
        | BoolVal(_), NumVal(_) -> NumVal(to_num val1 -. to_num val2)
        | StrVal(_), StrVal(_) -> StrVal(to_str val1 ^ to_str val2)
        | StrVal(_), (_) -> StrVal(to_str val1 ^ to_str val2)
        | (_), StrVal(_) -> StrVal(to_str val1 ^ to_str val2)
        | otherwise -> UndefVal
      )
    )

    | TimesBop -> (let val1 = (eval_expr env e1) in let val2 = (eval_expr env e2) in
      (match val1, val2 with
        | NumVal(_), NumVal(_) -> NumVal(to_num val1 *. to_num val2)
        | otherwise -> UndefVal
      )
    )

    | DivBop -> (let val1 = (eval_expr env e1) in let val2 = (eval_expr env e2) in
      NumVal(to_num val1 /. to_num val2)
    )

    | LtBop -> (let val1 = (eval_expr env e1) in let val2 = (eval_expr env e2) in
      (match val1, val2 with
        | StrVal(_), StrVal(_) -> if (to_str val1 >= to_str val2) then BoolVal(false) else BoolVal(true)
        | NumVal(_), NumVal(_) -> if (to_num val1 >= to_num val2) then BoolVal(false) else BoolVal(true)
        | _ -> UndefVal
      )
    )

    | LtBop -> (let val1 = (eval_expr env e1) in let val2 = (eval_expr env e2) in
      (match val1, val2 with
        | StrVal(_), StrVal(_) -> if (to_str val1 > to_str val2) then BoolVal(false) else BoolVal(true)
        | NumVal(_), NumVal(_) -> if (to_num val1 > to_num val2) then BoolVal(false) else BoolVal(true)
        | _ -> UndefVal
      )
    )

    | GtBop -> (let val1 = (eval_expr env e1) in let val2 = (eval_expr env e2) in
      (match val1, val2 with
        | StrVal(_), StrVal(_) -> if (to_str val1 <= to_str val2) then BoolVal(false) else BoolVal(true)
        | NumVal(_), NumVal(_) -> if (to_num val1 <= to_num val2) then BoolVal(false) else BoolVal(true)
        | _ -> UndefVal
      )
    )

    | GteBop -> (let val1 = (eval_expr env e1) in let val2 = (eval_expr env e2) in
      (match val1, val2 with
        | StrVal(_), StrVal(_) -> if (to_str val1 < to_str val2) then BoolVal(false) else BoolVal(true)
        | NumVal(_), NumVal(_) -> if (to_num val1 < to_num val2) then BoolVal(false) else BoolVal(true)
        | _ -> UndefVal
      )
    )

    | NeqBop -> (let val1 = (eval_expr env e1) in let val2 = (eval_expr env e2) in
      if (to_num val1 = to_num val2) then BoolVal(false) else BoolVal(true)
    )

    | EqBop -> (match e1, e2 with
      | ValExpr(_, BoolVal(b1)), ValExpr(_, BoolVal(b2)) -> (
        let val1 = (eval_expr env e1) in let val2 = (eval_expr env e2) in
          if (to_bool val1 = to_bool val2) then BoolVal(true) else BoolVal(false)
      )
      | ValExpr(_, NumVal(n1)), ValExpr(_, NumVal(n2)) -> (
        let val1 = (eval_expr env e1) in let val2 = (eval_expr env e2) in
          if (to_num val1 = to_num val2) then BoolVal(true) else BoolVal(false)
      )
      | _ -> UndefVal
    )
  )

  | PrintExpr(_,e) -> let _ = print_endline(to_str(eval_expr env e)) in UndefVal

  | FuncExpr(_, l) -> (match l with
    | (Some(n), l, b, y) -> ClosureVal(env, (Some(n), l, b, y))
    | (None, l, b, y) -> ClosureVal(env, (None, l, b, y)))

  | CallExpr(_, e1, expList) -> (match eval_expr env e1 with
    | ClosureVal (nmap, lambda) -> (let e2 = restore_environment env nmap in (match lambda with 
      | (v, vList, b, _) -> (let rec bind (env: environment_t) (vals: expr_t list) (nameList: typed_ident_t list) : environment_t =
        (match nameList, vals with
          | (env1, env2)::x1, env3::x2 -> bind(push_environment env env1 Immutable (eval_expr env env3)) x2 x1
          | ([], []) -> env
        )
        in eval_block (bind e2 expList vList) b)
      | _ -> UndefVal
    ))
    | _ -> UndefVal
  )

  

    
(*********)
(* Tests *)
(*********)

let test_group name tests =
  (name, compose (eval empty_env) parse_string, eq_value, eq_exn,
   Some((fun (x : string) -> x),str_value),
   (* None, *)
   tests)

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
(*let _ = Printf.printf "RESULT = %s\n" (str_program (parse_string "const f = function(x,y){return x+y;}; f(1,2)"))*)
let func_eval_tests =
  test_group "Function Definition Evaluation"
    [
      (* TODO *)
      (None, "function test(x){const x = \"abc\"; return x;}",
      Ok(ClosureVal(StringMap.empty,(
               Some("test"),
               [("x",None)],
               StmtBlock(NoPos,
                         ConstStmt(NoPos,
                                   "x",
                                   ValExpr(NoPos,StrVal("abc"))),
                         ReturnBlock(NoPos,VarExpr(NoPos,"x"))),
               None))));
      (Some("Two Parameter"), "function f(x,y){return x+y;}",
      Ok(ClosureVal(StringMap.empty, (
               Some("f"),
               [("x",None);("y",None)],
               ReturnBlock(NoPos,BopExpr(NoPos,VarExpr(NoPos,"x"),PlusBop,VarExpr(NoPos,"y"))),
               None))));
    ]

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

let call_eval_tests =
  test_group "Call Evaluation"
    [
      (Some("Simple Two Parameter Function"),
       "const f = function(x,y){return x+y;}; f(1,2)",
       Ok(NumVal(3.0)));
      (Some("Simple Three Parameter Function"),
       "const f = function(x,y,z){return x+y+z;}; f(1,2,3)",
       Ok(NumVal(6.0)));
      (Some("Function Call Inside Different Function"),
       "const f = function(x){return function(y){return x+y;};}; const g = f(1); g(2)",
       Ok(NumVal(3.0)));
      (None, "const f = function(x){return x*9*7/2;}; f(2)",                    Ok(NumVal(63.0)));
      (None, "const f = function(x,y){return x && y;}; f(true, false)",       Ok(BoolVal(false)));
      (None, "const f = function(x){return x+1-2+3-4*5;}; f(1)",       Ok(NumVal(-17.0)));
      (None, "const z = false; const f = function(y,z){return !z && y;}; f(true,true)", Ok(BoolVal(false)));
      (None, "const f = function(x,y,z){return x*y*z;}; f(-2,6,-9.0)", Ok(NumVal(108.0)));
    ]
