open Javascript_parser;;
open Javascript_lexer;;
open Javascript_ast;;

let get_ast (i : in_channel) =
   Javascript_parser.start Javascript_lexer.token (Lexing.from_channel i)

(* extract program from a AST start *)
let get_prog s =
  match s with
    JavascriptProgram(_,p) -> p

(* extract expression from a single-expression program *)
let get_expr p =
  match p with
    ExprProgram(_,e) -> e
  | _ -> failwith "Not an Expression"


let rec tokenize lexbuf =
  let tok = (token lexbuf)
  in match tok with
       EOF -> [tok]
     | _ -> tok :: (tokenize lexbuf)



let parse_string (s : string) : program_t =
  match Javascript_parser.start
          Javascript_lexer.token (Lexing.from_string s) with
  | JavascriptProgram(_,p) -> p


(* parse input string as an expression *)
let parse_expr (s : string) : expr_t =
  get_expr (parse_string s)



let str_token x =
  match x with
   EOF -> "EOF"
  | SEMICOLON_OP -> ";"
  | SUB_OP -> "-"
  | LT_OP -> "<"
  | GT_OP -> ">"
  | COND_OP -> "?"
  | COLON_OP -> ":"
  | LP_KW -> "("
  | DOT_OP -> "."
  | LOG_NOT_OP -> "!"
  | MUL_OP -> "*"
  | DIV_OP -> "/"
  | ADD_OP -> "+"
  | COMMA_OP  -> ","
  | LSB_KW -> "["
  | RSB_KW -> "]"
  | RP_KW -> ")"
  | LCB_KW -> "{"
  | RCB_KW -> "}"
  | ASSIGN_OP -> "="
  | NUMBER (f) -> Printf.sprintf "NUMBER(%f)" f
  | LEQ_OP -> "<="
  | GEQ_OP -> ">="
  | STREQ_OP -> "==="
  | NSTREQ_OP -> "!=="
  | LOG_AND_OP -> "&&"
  | LOG_OR_OP -> "||"
  | CONSOLE_KW -> "console"
  | LOG_KW -> "log"
  | IDENT (s) -> Printf.sprintf "IDENT(%s)" s
  | STRING (s) -> Printf.sprintf "STRING(%s)" s
  | NAN_KW -> "NaN"
  | INFINITY_KW -> "Infinity"
  | BLANKS -> "BLANKS"
  | CONST_KW -> "CONST_KW"
  | FALSE_KW -> "FALSE_KW"
  | FUNC_KW -> "FUNC_KW"
  | LET_KW -> "LET_KW"
  | MUL_OPTI_LINE_COMMENT -> "MUL_OPTI_LINE_COMMENT"
  | RET_KW -> "RET_KW"
  | SINGLE_LINE_COMMENT -> "SINGLE_LINE_COMMENT"
  | TRUE_KW -> "TRUE_KW"
  | UNDEF_KW -> "UNDEF_KW"


let str_token_list l =
  "["^(Util.str_x_list str_token l "; ")^"]"

(* Main Driver *)
let arg_input = ref "-"
let arg_test = ref false

let args =
  Arg.align [
      ("--test",
       Arg.Unit(fun x -> arg_test := true),
       " Run the tests");
    ]

let channel_driver name eval_fun test_fun =
  Arg.parse
    args
    (fun x -> arg_input := x)
    (Printf.sprintf "%s [--test] [INPUT]" name);
  if !arg_test then test_fun ()
  else eval_fun (if "-" = !arg_input then stdin
                 else open_in !arg_input)

let prog_driver name eval_fun test_fun =
  channel_driver name
    (fun channel ->
      eval_fun (get_prog (get_ast channel)))
    test_fun
