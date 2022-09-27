open Javascript_ast
open Javascript_main

(*
 * Check javascript_ast.ml for the following useful functionality:
 * - str_float               -- convert a float to a string
 * - to_num, to_bool, to_str -- do the JavaScript automatic type conversion
 * - read_environment        -- look up a variable's value in the environment
 * - push_environment        -- add a variable binding to the environment
 * - empty_env               -- the empty environment
 *)

(* basic tests to show how the value conversion functions work (do not modify) *)
let simple_to_num_tests =
  ("Simple ToNum Conversions", to_num, (fun n1 n2 -> eq_float (n1,n2)), eq_exn, Some(str_value,str_float),
   [
     (None, NumVal(123.0), Ok(123.0));
     (None, BoolVal(true), Ok(1.0));
     (None, StrVal(""),    Ok(0.0));
  ])
let simple_to_bool_tests =
  ("Simple ToBool Conversions", to_bool, (=), eq_exn, Some(str_value,string_of_bool),
   [
     (None, BoolVal(true),  Ok(true));
     (None, NumVal(1.0),    Ok(true));
     (None, StrVal("true"), Ok(true));
  ])
let simple_to_str_tests =
  ("Simple ToStr Conversions", to_str, (=), eq_exn, Some(str_value,(fun x -> x)),
   [
     (None, StrVal("hello"), Ok("hello"));
     (None, BoolVal(true),   Ok("true"));
     (None, NumVal(1.234),   Ok("1.234"));
     (None, NumVal(1.000),   Ok("1"));
     (None, NumVal(0.00),    Ok("0"));
     (None, NumVal(100.01),  Ok("100.01"));
  ])

(*
 * (eval env p) should reduce a program in initial environment env to
 * a *value* (if Node.js produces a *value* for an example JavaScript
 * program, your evaluator should produce that same value).
 *
 *)

(* evaluate a program *)
(* TODO *)
let rec eval (env : environment_t) (p : program_t) : value_t = match p with
(* add action to the statment calls *)
(* operational *)
  | ExprProgram(_,e) -> eval_expr env e
  | StmtProgram(_,s,p) -> eval (eval_stmt env s) p

(* evaluate a statement *)
(* See: Javascript_ast.push_environment and Javascript_ast.read_environment *)
(* TODO *)
and eval_stmt (env:environment_t) (s:stmt_t) : environment_t = match s with
(* add action to the statment calls *)
  | ConstStmt(_,v,e) -> push_environment env v Immutable (eval_expr env e)
  (* dont need to to let and assign until next lab *)
  (* |LetStmt(_,v,e) -> push_environment env s Immutable (eval_expr env s)
  |AssignStmt(_,e1,e2) -> push_environment env s Mutable (eval_expr env s) *)
  | _ -> raise (UnimplementedStmt(s))

(* evaluate a value *)
(* TODO *)
and eval_expr (env:environment_t) (e:expr_t) : value_t =  match e with
  | ValExpr(_,v) -> v

  | VarExpr(p,v) -> (let x = read_environment env v in
    match x with
    | Some(Immutable, NumVal num) -> NumVal(num)
  )


  (* Evaluation of Block expressions. *)  
  | BlockExpr(_,b) -> (match b with
    | StmtBlock(p,s,b) -> StrVal("Test Block")
    | ReturnBlock(p,e) -> eval_expr env e
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
        | BoolVal(_), BoolVal(_) -> BoolVal(to_bool val1 && to_bool val2)
        | NumVal(_), NumVal(_) -> (match to_bool val1, to_bool val2 with
          | true, true -> NumVal(to_num val1)
          | true, false -> NumVal(to_num val2)
          | false, true -> NumVal(to_num val1)
          | false, false -> NumVal(to_num val1)
        | _ -> UndefVal
        )
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
        | _ -> UndefVal
        )
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

(*********)
(* Tests *)
(*********)

let test_group name tests =
  (name, (fun p -> eval empty_env p), eq_value, eq_exn, Some(str_program,str_value), tests)


(* basic tests for the evaluator (do not modify) *)
let simple_expr_eval_tests =
  test_group "Simple Expression Evaluation"
    [
      (None, parse_string "1 + true",                    Ok(NumVal(2.0)));
      (None, parse_string "false + true",                Ok(NumVal(1.0)));
      (None, parse_string "100 || 200",                  Ok(NumVal(100.0)));
      (None, parse_string "-false",                      Ok(NumVal(0.0)));
      (None, parse_string "1 + 1",                       Ok(NumVal(2.0)));
      (None, parse_string "3 + (4 + 5)",                 Ok(NumVal(12.0)));
      (None, parse_string "3 * (4 + 5)",                 Ok(NumVal(27.0)));
      (None, parse_string "-6 * 90 - 8",                 Ok(NumVal(-548.0)));
      (None, parse_string "-100 + 50",                   Ok(NumVal(-50.0)));
      (None, parse_string "true && (false || true)",     Ok(BoolVal(true)));
      (None, parse_string "true && (false || !true)",    Ok(BoolVal(false)));
      (None, parse_string "1 < 2",                       Ok(BoolVal(true)));
      (None, parse_string "100 === 100",                 Ok(BoolVal(true)));
      (None, parse_string "100 === 101",                 Ok(BoolVal(false)));
      (None, parse_string "100 !== 200",                 Ok(BoolVal(true)));
      (None, parse_string "true === true",               Ok(BoolVal(true)));
      (None, parse_string "0 / 0",                       Ok(NumVal(nan)));
    ]

let simple_print_eval_tests =
  test_group "Simple Print Evaluation"
    [
      (None, parse_string "console.log(\"Hello World\")",           Ok(UndefVal));
    ]

let simple_cond_eval_tests =
  test_group "Simple Conditional Evaluation"
    [
      (None, parse_string "(1 < 2) ? 123 : 124",         Ok(NumVal(123.0)));
    ]

let simple_str_eval_tests =
  test_group "Simple String Evaluation"
    [
      (None, parse_string "\"aaa\" < \"aaaa\"",          Ok(BoolVal(true)));
      (None, parse_string "\"bbb\" < \"aaa\"",           Ok(BoolVal(false)));
      (None, parse_string "\"hello\"+\" \"+\"world\"",   Ok(StrVal("hello world")));
    ]

let simple_var_eval_tests =
  test_group "Simple Variable Evaluation"
    [
      (None, parse_string "const x = 1; x+1",            Ok(NumVal(2.0)));
      (None, parse_string "const x=1; const y=2; x+y",   Ok(NumVal(3.0)));
      (None, parse_string "const x=3; const y=x*2+1; y", Ok(NumVal(7.0)));
    ]

let cond_eval_tests =
  test_group "Conditional Evaluation"
    [
      (* TODO *)
      (None, parse_string "(((2 >= 5) && (true === true)) || ((2 * 3 * 4 * 5) < 200))", Ok(BoolVal(true)));
      (None, parse_string "((1 <= 1) && (true)) ? \"true\" : \"false\"",       Ok(StrVal("True")));
      (None, parse_string "(((2 >= 5) && (true === true)) || ((2 * 3 * 4 * 5) < 200))", Ok(BoolVal(true)));
    ]

let str_eval_tests =
  test_group "String Evaluation"
    [
      (*
      (None, parse_string "\"hello\" = \"world\"", Ok(BoolVal(false)));
      (None, parse_string "\"hello\" < \"world\"", Ok(BoolVal(true)));
      (None, parse_string "\"hello\" + \" \" + \"world\"", Ok(StrVal("hello world")));
       *)
    ]

let var_eval_tests =
  test_group "Variable Evaluation"
    [
    (*
    (None, parse_string "const x = 4; const y = x^2 + 1; y", Ok(NumVal(17.0)));
      (None, parse_string "const x="Hello "; const y="World"; const z= x + y; z", Ok(StrVal("Hello World")));
      (None, parse_string "const x=true; const y= false + false; y", Ok(BoolVal(true)));
      *)
    ]