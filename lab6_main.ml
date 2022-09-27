open Javascript_ast;;
open Javascript_main;;
open Testing;;
open Util;;

let eval_prog p =
  let e = get_expr p in
  Printf.printf "result: %s\ntype: %s\n"
    (str_value (Lab6.eval e) )
    (str_option str_typ (Lab6.typecheck e))
;;

let run_tests () =
  print_string "Running Lab 6 Tests\n";
  print_string "===================\n";
  print_tests Lab6.simple_eval_tests;
  print_tests Lab6.simple_typecheck_tests;
  print_tests Lab6.eval_tests;
  print_tests Lab6.typecheck_tests
;;

(* Main *)
prog_driver "lab6" eval_prog run_tests
