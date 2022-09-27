open Javascript_parser;;
open Javascript_lexer;;
open Javascript_main;;
open Javascript_ast;;
open Util;;


let print_tokens_channel i =
  let lexbuf = (Lexing.from_channel i) in
  let tokens = tokenize lexbuf in
  print_string (str_token_list tokens); print_string "\n"

let print_ast_channel channel =
  print_string (str_ast (get_ast channel));
  print_string "\n"

let run_tests () =
  print_string "Running Lab 5 Tests\n";
  print_string "===================\n";
  Testing.print_tests Lab5_tests.lexer_tests;
  Testing.print_tests Lab5_tests.parser_tests



let arg_input = ref "-"
let arg_test = ref false
let arg_lex = ref false
let arg_parse = ref false

let args =
  Arg.align [
      ("--test",
       Arg.Unit(fun x -> arg_test := true),
       " Run the tests");
      ("--lex",
       Arg.Unit(fun x -> arg_lex := true),
       " Lex the input");
      ("--parse",
       Arg.Unit(fun x -> arg_parse := true),
       " Parse the input");
    ]

let usage_msg = "lab5 OPTION input"

(* Define a function for the main entry point *)
let main () =
  Arg.parse args (fun x -> arg_input := x) usage_msg;
  (* Printf.printf "Input: %s\n" !arg_input; *)
  if !arg_test then run_tests ()
  else if !arg_lex then print_tokens_channel (open_input !arg_input)
  else if !arg_parse then print_ast_channel (Util.open_input !arg_input)
  else print_string "ERROR: No option specified.\n"
;;

(* Call main *)
main ()
