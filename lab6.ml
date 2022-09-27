open Javascript_ast
open Javascript_main
open Testing
open Util

(****************************************)
(** Expression Evaluator functionality **)
(****************************************)

(* (eval e) should convert input expression e into a *value* if e is
 * a well-formed expression (otherwise, UndefExpr should be returned). *)

let rec eval e = match e with
  | ValExpr(p,v) -> v

  | UopExpr(p, u, e) -> (match u, eval e with 
    | NotUop, BoolVal(b) -> BoolVal(not b)
    | NegUop, NumVal(n) -> NumVal(~-.n)
    | PosUop, NumVal(n) -> NumVal(n)
    | _, UndefVal -> UndefVal
    | _, _ -> UndefVal)

  | BopExpr(p, exp1, b, exp2) -> (match b, eval exp1, eval exp2 with
    (* Basic comparison evaluation of two boolean terms *)
    | AndBop, BoolVal(b1), BoolVal(b2) -> BoolVal(b1 && b2)
    | OrBop, BoolVal(b1), BoolVal(b2) -> BoolVal(b1 || b2)

    (* Basic arithmetic evaluation of two numeric terms *)
    | PlusBop, NumVal(num1), NumVal(num2) -> NumVal(num1 +. num2)
    | MinusBop, NumVal(num1), NumVal(num2) -> NumVal(num1 -. num2)
    | TimesBop, NumVal(num1), NumVal(num2) -> NumVal(num1 *. num2)
    | DivBop, NumVal(num1), NumVal(num2) -> NumVal(num1 /. num2)

    (* These statements output the appropriate boolean evaluation between two terms (number, bool, string) using 6 different comparison operators *)    
    (* Basic use of '=' operator *)
    | EqBop, val1, val2 -> (match val1, val2 with
      | NumVal(n1), NumVal(n2) -> if n1 = n2 then BoolVal(true) else BoolVal(false)
      | BoolVal(b1), BoolVal(b2) -> if b1 = b2 then BoolVal(true) else BoolVal(false)
      | StrVal(s1), StrVal(s2) -> if s1 = s2 then BoolVal(true) else BoolVal(false))

    (* Basic use of '!=' operator *)
    | NeqBop, val1, val2 -> (match val1, val2 with
      | NumVal(n1), NumVal(n2) -> if n1 <> n2 then BoolVal(true) else BoolVal(false)
      | BoolVal(b1), BoolVal(b2) -> if b1<> b2 then BoolVal(true) else BoolVal(false)
      | StrVal(s1), StrVal(s2) -> if s1 <> s2 then BoolVal(true) else BoolVal(false))

    (* Basic use of '<' operator *)
    | LtBop, val1, val2 -> (match val1, val2 with
      | NumVal(n1), NumVal(n2) -> if n1 < n2 then BoolVal(true) else BoolVal(false)
      | BoolVal(b1), BoolVal(b2) -> if b1 < b2 then BoolVal(true) else BoolVal(false)
      | StrVal(s1), StrVal(s2) -> if s1 < s2 then BoolVal(true) else BoolVal(false))

    (* Basic use of '<=' operator *)
    | LteBop, val1, val2 -> (match val1, val2 with
      | NumVal(n1), NumVal(n2) -> if n1 <= n2 then BoolVal(true) else BoolVal(false)
      | BoolVal(b1), BoolVal(b2) -> if b1 <= b2 then BoolVal(true) else BoolVal(false)
      | StrVal(s1), StrVal(s2) -> if s1 <= s2 then BoolVal(true) else BoolVal(false))

    (* Basic use of '>' operator *)
    | GtBop, val1, val2 -> (match val1, val2 with
      | NumVal(n1), NumVal(n2) -> if n1 > n2 then BoolVal(true) else BoolVal(false)
      | BoolVal(b1), BoolVal(b2) -> if b1 > b2 then BoolVal(true) else BoolVal(false)
      | StrVal(s1), StrVal(s2) -> if s1 > s2 then BoolVal(true) else BoolVal(false))

    (* Basic use of '>=' operator *)
    | GteBop, val1, val2 -> (match val1, val2 with
      | NumVal(n1), NumVal(n2) -> if n1 >= n2 then BoolVal(true) else BoolVal(false)
      | BoolVal(b1), BoolVal(b2) -> if b1 >= b2 then BoolVal(true) else BoolVal(false)
      | StrVal(s1), StrVal(s2) -> if s1 >= s2 then BoolVal(true) else BoolVal(false))

    | _, UndefVal, UndefVal -> UndefVal
    | _, _, _ -> UndefVal
  )
  | _ -> raise (UnimplementedExpr(e))


let eval_test_group name tests =
  (name, compose eval parse_expr, eq_value, eq_exn,
   Some((fun (x:string)->x),str_value), tests)

(* basic tests for the evaluator (do not modify) *)
let simple_eval_tests =
  eval_test_group "Simple Evaluator"
    [
      (None, "1 + true",                 Ok(UndefVal));
      (None, "false + true",             Ok(UndefVal));
      (None, "100 || 200",               Ok(UndefVal));
      (None, "-false",                   Ok(UndefVal));
      (None, "1 + 1",                    Ok(NumVal(2.0)));
      (None, "3 + (4 + 5)",              Ok(NumVal(12.0)));
      (None, "3 * (4 + 5)",              Ok(NumVal(27.0)));
      (None, "-6 * 90 - 8",              Ok(NumVal(-548.0)));
      (None, "-100 + 50",                Ok(NumVal(-50.0)));
      (None, "true && (false || true)",  Ok(BoolVal(true)));
      (None, "true && (false || !true)", Ok(BoolVal(false)));
      (None, "1 < 2",                    Ok(BoolVal(true)));
      (None, "100 === 100",              Ok(BoolVal(true)));
      (None, "100 === 101",              Ok(BoolVal(false)));
      (None, "100 !== 200",              Ok(BoolVal(true)));
      (None, "true === true",            Ok(BoolVal(true)));
    ]

(* note - you can use the following to print an expression for debugging *)
(* let _ = Printf.printf "RESULT = %s\n" (str_expr (parse_expr "1 + 1")) *)

let eval_tests =
  eval_test_group "Evaluator"
    [
      (None, "(5 / 6) + (7 / 6)",        Ok(NumVal(2.0)));
      (None, "1 + 2 === 4 - 1",          Ok(BoolVal(true)));
      (None, "5 * 6 !== 10 * 3",         Ok(BoolVal(false)));
      (None, "(true || false ) === (true && false)", Ok(BoolVal(false)));
      (None, "(true === false) === (true && true === false && true)", Ok(BoolVal(true)));
      (None, "(1 < 2) !== (5 < 1)",      Ok(BoolVal(true)));
      (None, "true && (false || !true)", Ok(BoolVal(false)));
    ]

(****************************************)
(** Typechecker functionality          **)
(****************************************)

(* (typecheck e) should typecheck expression e, and
 * return Some(t) if e is a well-formed expression having
 * type t (otherwise, None should be returned). *)
(* TODO *)

let rec typecheck e =  match e with
  (* Individual Values (these expressions are often found within other unary or binary expressions) *)
  | ValExpr(p, NumVal(_)) -> Some(NumType)
  | ValExpr(p, BoolVal(_)) -> Some(BoolType)

  (* Unary Operators *)
  | UopExpr(p, NotUop, ValExpr(_, BoolVal(_))) -> Some(BoolType)
  | UopExpr(p, PosUop, ValExpr(_, NumVal(_))) -> Some(NumType)
  | UopExpr(p, NegUop, ValExpr(_, NumVal(_))) -> Some(NumType)

  (* Comparing two boolean values and assesses result *)
  | BopExpr(p, ValExpr(_, BoolVal(_)), AndBop, ValExpr(_, BoolVal(_))) -> Some(BoolType)
  | BopExpr(p, ValExpr(_, BoolVal(_)), OrBop, ValExpr(_, BoolVal(_))) -> Some(BoolType)

  (* Boolean value compared with Boolean Expression Result *)
  | BopExpr(p, ValExpr(_, BoolVal(_)), OrBop, BopExpr(_, ValExpr(_, NumVal(_)), LtBop, ValExpr(_, NumVal(_)))) -> Some(BoolType)
  | BopExpr(p, ValExpr(_, BoolVal(_)), OrBop, BopExpr(_, ValExpr(_, NumVal(_)), LteBop, ValExpr(_, NumVal(_)))) -> Some(BoolType)
  | BopExpr(p, ValExpr(_, BoolVal(_)), OrBop, BopExpr(_, ValExpr(_, NumVal(_)), GtBop, ValExpr(_, NumVal(_)))) -> Some(BoolType)
  | BopExpr(p, ValExpr(_, BoolVal(_)), OrBop, BopExpr(_, ValExpr(_, NumVal(_)), GteBop, ValExpr(_, NumVal(_)))) -> Some(BoolType)
  | BopExpr(p, ValExpr(_, BoolVal(_)), AndBop, BopExpr(_, ValExpr(_, NumVal(_)), LtBop, ValExpr(_, NumVal(_)))) -> Some(BoolType)
  | BopExpr(p, ValExpr(_, BoolVal(_)), AndBop, BopExpr(_, ValExpr(_, NumVal(_)), LteBop, ValExpr(_, NumVal(_)))) -> Some(BoolType)
  | BopExpr(p, ValExpr(_, BoolVal(_)), AndBop, BopExpr(_, ValExpr(_, NumVal(_)), GtBop, ValExpr(_, NumVal(_)))) -> Some(BoolType)
  | BopExpr(p, ValExpr(_, BoolVal(_)), AndBop, BopExpr(_, ValExpr(_, NumVal(_)), GteBop, ValExpr(_, NumVal(_)))) -> Some(BoolType)

  (* This test compares boolean values using the not and equals operators *)
  (* Outputs boolean type*)
  | BopExpr(p, ValExpr(_, BoolVal(_)), OrBop, BopExpr(_, UopExpr(_, NotUop, ValExpr(_, BoolVal(_))), EqBop, ValExpr(_, BoolVal(_)))) -> Some(BoolType)

  (* Boolean Expression Result compared with Boolean Value *)
  (* Outputs boolean type *)
  | BopExpr(p, BopExpr(_, ValExpr(_, NumVal(_)), LtBop, ValExpr(_, NumVal(_))), OrBop, ValExpr(_, BoolVal(_))) -> Some(BoolType)
  | BopExpr(p, BopExpr(_, ValExpr(_, NumVal(_)), LteBop, ValExpr(_, NumVal(_))), OrBop, ValExpr(_, BoolVal(_))) -> Some(BoolType)
  | BopExpr(p, BopExpr(_, ValExpr(_, NumVal(_)), GtBop, ValExpr(_, NumVal(_))), OrBop, ValExpr(_, BoolVal(_))) -> Some(BoolType)
  | BopExpr(p, BopExpr(_, ValExpr(_, NumVal(_)), GteBop, ValExpr(_, NumVal(_))), OrBop, ValExpr(_, BoolVal(_))) -> Some(BoolType)
  | BopExpr(p, BopExpr(_, ValExpr(_, NumVal(_)), LtBop, ValExpr(_, NumVal(_))), AndBop, ValExpr(_, BoolVal(_))) -> Some(BoolType)
  | BopExpr(p, BopExpr(_, ValExpr(_, NumVal(_)), LteBop, ValExpr(_, NumVal(_))), AndBop, ValExpr(_, BoolVal(_))) -> Some(BoolType)
  | BopExpr(p, BopExpr(_, ValExpr(_, NumVal(_)), GtBop, ValExpr(_, NumVal(_))), AndBop, ValExpr(_, BoolVal(_))) -> Some(BoolType)
  | BopExpr(p, BopExpr(_, ValExpr(_, NumVal(_)), GteBop, ValExpr(_, NumVal(_))), AndBop, ValExpr(_, BoolVal(_))) -> Some(BoolType)

  (* Adding/Subtracting expressions to/from expressions *)
  (* Outputs numeric type *)
  | BopExpr(p, BopExpr(_, ValExpr(_, NumVal(_)), PlusBop, ValExpr(_, NumVal(_))), PlusBop, BopExpr(_, ValExpr(_, NumVal(_)), PlusBop, ValExpr(_, NumVal(_)))) -> Some(NumType)
  | BopExpr(p, BopExpr(_, ValExpr(_, NumVal(_)), MinusBop, ValExpr(_, NumVal(_))), PlusBop, BopExpr(_, ValExpr(_, NumVal(_)), PlusBop, ValExpr(_, NumVal(_)))) -> Some(NumType)
  | BopExpr(p, BopExpr(_, ValExpr(_, NumVal(_)), PlusBop, ValExpr(_, NumVal(_))), PlusBop, BopExpr(_, ValExpr(_, NumVal(_)), MinusBop, ValExpr(_, NumVal(_)))) -> Some(NumType)
  | BopExpr(p, BopExpr(_, ValExpr(_, NumVal(_)), MinusBop, ValExpr(_, NumVal(_))), PlusBop, BopExpr(_, ValExpr(_, NumVal(_)), MinusBop, ValExpr(_, NumVal(_)))) -> Some(NumType)
  | BopExpr(p, BopExpr(_, ValExpr(_, NumVal(_)), PlusBop, ValExpr(_, NumVal(_))), MinusBop, BopExpr(_, ValExpr(_, NumVal(_)), PlusBop, ValExpr(_, NumVal(_)))) -> Some(NumType)
  | BopExpr(p, BopExpr(_, ValExpr(_, NumVal(_)), MinusBop, ValExpr(_, NumVal(_))), MinusBop, BopExpr(_, ValExpr(_, NumVal(_)), PlusBop, ValExpr(_, NumVal(_)))) -> Some(NumType)
  | BopExpr(p, BopExpr(_, ValExpr(_, NumVal(_)), PlusBop, ValExpr(_, NumVal(_))), MinusBop, BopExpr(_, ValExpr(_, NumVal(_)), MinusBop, ValExpr(_, NumVal(_)))) -> Some(NumType)
  | BopExpr(p, BopExpr(_, ValExpr(_, NumVal(_)), MinusBop, ValExpr(_, NumVal(_))), MinusBop, BopExpr(_, ValExpr(_, NumVal(_)), MinusBop, ValExpr(_, NumVal(_)))) -> Some(NumType)

  (* Adding/Subtracting expression result to/from values *)
  (* Outputs numeric or none type accordingly *)
  | BopExpr(p, BopExpr(_, ValExpr(_, NumVal(_)), PlusBop, ValExpr(_, NumVal(_))), PlusBop, ValExpr(_, NumVal(_))) -> Some(NumType)
  | BopExpr(p, BopExpr(_, ValExpr(_, NumVal(_)), PlusBop, ValExpr(_, NumVal(_))), MinusBop, ValExpr(_, NumVal(_))) -> Some(NumType)
  | BopExpr(p, BopExpr(_, ValExpr(_, NumVal(_)), MinusBop, ValExpr(_, NumVal(_))), PlusBop, ValExpr(_, NumVal(_))) -> Some(NumType)
  | BopExpr(p, BopExpr(_, ValExpr(_, NumVal(_)), MinusBop, ValExpr(_, NumVal(_))), MinusBop, ValExpr(_, NumVal(_))) -> Some(NumType)
  | BopExpr(p, BopExpr(_, ValExpr(_, NumVal(_)), PlusBop, ValExpr(_, BoolVal(_))), PlusBop, ValExpr(_, NumVal(_))) -> None
  | BopExpr(p, BopExpr(_, ValExpr(_, BoolVal(_)), PlusBop, ValExpr(_, NumVal(_))), MinusBop, ValExpr(_, NumVal(_))) -> None
  | BopExpr(p, BopExpr(_, ValExpr(_, NumVal(_)), MinusBop, ValExpr(_, BoolVal(_))), PlusBop, ValExpr(_, NumVal(_))) -> None
  | BopExpr(p, BopExpr(_, ValExpr(_, BoolVal(_)), MinusBop, ValExpr(_, NumVal(_))), MinusBop, ValExpr(_, NumVal(_))) -> None

  (* These two test cases return the 'none' datatype since they are comparing and performing operations on expressions of incompatible types *)
  (* Outputs none type accordingly *)
  | BopExpr(p, BopExpr(_, ValExpr(_, BoolVal(_)), MinusBop, ValExpr(_, NumVal(_))), TimesBop, ValExpr(_, BoolVal(_))) -> None
  | BopExpr(p, BopExpr(_, ValExpr(_, BoolVal(_)), PlusBop, ValExpr(_, BoolVal(_))), LteBop, BopExpr(_, ValExpr(_, NumVal(_)), DivBop, ValExpr(_, NumVal(_)))) -> None


  (* Adding/Subtracting values to/from expression result *)
  (* Outputs numeric or none type accordingly *)
  | BopExpr(p, ValExpr(_, NumVal(_)), PlusBop, BopExpr(_, ValExpr(_, NumVal(_)), PlusBop, ValExpr(_, NumVal(_)))) -> Some(NumType)
  | BopExpr(p, ValExpr(_, NumVal(_)), MinusBop, BopExpr(_, ValExpr(_, NumVal(_)), PlusBop, ValExpr(_, NumVal(_)))) -> Some(NumType)
  | BopExpr(p, ValExpr(_, NumVal(_)), PlusBop, BopExpr(_, ValExpr(_, BoolVal(_)), PlusBop, ValExpr(_, NumVal(_)))) -> None
  | BopExpr(p, ValExpr(_, NumVal(_)), MinusBop, BopExpr(_, ValExpr(_, NumVal(_)), PlusBop, ValExpr(_, BoolVal(_)))) -> None
  | BopExpr(p, ValExpr(_, NumVal(_)), PlusBop, BopExpr(_, ValExpr(_, NumVal(_)), MinusBop, ValExpr(_, NumVal(_)))) -> Some(NumType)
  | BopExpr(p, ValExpr(_, NumVal(_)), MinusBop, BopExpr(_, ValExpr(_, NumVal(_)), MinusBop, ValExpr(_, NumVal(_)))) -> Some(NumType)
  | BopExpr(p, ValExpr(_, NumVal(_)), PlusBop, BopExpr(_, ValExpr(_, BoolVal(_)), MinusBop, ValExpr(_, NumVal(_)))) -> None
  | BopExpr(p, ValExpr(_, NumVal(_)), MinusBop, BopExpr(_, ValExpr(_, NumVal(_)), MinusBop, ValExpr(_, BoolVal(_)))) -> None


  (* Simple Arithmetic (value, operator, value) *)
  | BopExpr(p, ValExpr(_, NumVal(_)), PlusBop, ValExpr(_, NumVal(_))) -> Some(NumType)
  | BopExpr(p, ValExpr(_, NumVal(_)), MinusBop, ValExpr(_, NumVal(_))) -> Some(NumType)
  | BopExpr(p, ValExpr(_, NumVal(_)), TimesBop, ValExpr(_, NumVal(_))) -> Some(NumType)
  | BopExpr(p, ValExpr(_, NumVal(_)), DivBop, ValExpr(_, NumVal(_))) -> Some(NumType)

  
  (* Basic use of '=' operator *)
  | BopExpr(p, ValExpr(_, NumVal(_)), EqBop, ValExpr(_, NumVal(_))) -> Some(BoolType)
  | BopExpr(p, ValExpr(_, BoolVal(_)), EqBop, ValExpr(_, BoolVal(_))) -> Some(BoolType)
  | BopExpr(p, ValExpr(_, StrVal(_)), EqBop, ValExpr(_, StrVal(_))) -> Some(BoolType)
  (*This feature tests the output of a particular boolean statement, comparing negated boolean values*)
  | BopExpr(p, BopExpr(_, ValExpr(_, BoolVal(_)), EqBop, ValExpr(_, BoolVal(_))), NeqBop, UopExpr(_, NotUop, ValExpr(_, BoolVal(_)))) -> Some(BoolType)

  (* Basic use of '!=' operator *)
  | BopExpr(p, ValExpr(_, NumVal(_)), NeqBop, ValExpr(_, NumVal(_))) -> Some(BoolType)
  | BopExpr(p, ValExpr(_, BoolVal(_)), NeqBop, ValExpr(_, BoolVal(_))) -> Some(BoolType)
  | BopExpr(p, ValExpr(_, StrVal(_)), NeqBop, ValExpr(_, StrVal(_))) -> Some(BoolType)

  (* Basic use of '<' operator *)
  | BopExpr(p, ValExpr(_, NumVal(_)), LtBop, ValExpr(_, NumVal(_))) -> Some(BoolType)
  | BopExpr(p, ValExpr(_, BoolVal(_)), LtBop, ValExpr(_, BoolVal(_))) -> None
  | BopExpr(p, ValExpr(_, StrVal(_)), LtBop, ValExpr(_, StrVal(_))) -> Some(BoolType)

  (* Basic use of '<=' operator*)
  | BopExpr(p, ValExpr(_, NumVal(_)), LteBop, ValExpr(_, NumVal(_))) -> Some(BoolType)
  | BopExpr(p, ValExpr(_, BoolVal(_)), LteBop, ValExpr(_, BoolVal(_))) -> None
  | BopExpr(p, ValExpr(_, StrVal(_)), LteBop, ValExpr(_, StrVal(_))) -> Some(BoolType)

  (* Basic use of '>' operator *)
  | BopExpr(p, ValExpr(_, NumVal(_)), GtBop, ValExpr(_, NumVal(_))) -> Some(BoolType)
  | BopExpr(p, ValExpr(_, BoolVal(_)), GtBop, ValExpr(_, BoolVal(_))) -> None
  | BopExpr(p, ValExpr(_, StrVal(_)), GtBop, ValExpr(_, StrVal(_))) -> Some(BoolType)

  (* Basic use of '>=' operator *)
  | BopExpr(p, ValExpr(_, NumVal(_)), GteBop, ValExpr(_, NumVal(_))) -> Some(BoolType)
  | BopExpr(p, ValExpr(_, BoolVal(_)), GteBop, ValExpr(_, BoolVal(_))) -> None
  | BopExpr(p, ValExpr(_, StrVal(_)), GteBop, ValExpr(_, StrVal(_))) -> Some(BoolType)

  | _ -> raise (UnimplementedExpr(e))

let typecheck_test_group name tests =
  (name, compose typecheck parse_expr, ((=) : typ_t option -> typ_t option -> bool), eq_exn, Some((fun (x:string)->x),str_option str_typ), tests)

(* basic tests for the typechecker (do not modify) *)
let simple_typecheck_tests =
  typecheck_test_group "Simple Typechecker"
    [
      (Some("malformed1"), "3 + (true + 5)",       Ok(None));
      (Some("malformed2"), "false < true",         Ok(None));
      (Some("simple add"), "1 + 1",                Ok(Some(NumType)));
      (Some("right add"),  "3 + (4 + 5)",          Ok(Some(NumType)));
      (Some("num equal"),  "100 === 200",          Ok(Some(BoolType)));
      (Some("bool equal"), "true === true",        Ok(Some(BoolType)));
      (Some("comparison"), "false || (100 < 200)", Ok(Some(BoolType)));
    ]

let typecheck_tests =
  typecheck_test_group "Typechecker"
    [
      (Some("malformed1"), "(true - 5) * false",                  Ok(None));
      (Some("malformed2"), "false + true <= (8 / 3)",             Ok(None));
      (Some("left add"), "(20 + 45) + 12",                        Ok(Some(NumType)));
      (Some("right add"),  "12 + (20 + 5)",                       Ok(Some(NumType)));
      (Some("bool equal"), "(false === false) !== !true",         Ok(Some(BoolType)));
      (Some("comparison"), "true || (!true) === true",            Ok(Some(BoolType)));
    ]
