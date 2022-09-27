open Javascript_ast;;
open Javascript_main;;
open Testing;;

let eval_prog p =
  Printf.printf "result: %s\n"
    (str_value (Lab8.eval empty_env p))
;;

let run_tests () =
  print_string "Running Lab 8 Tests\n";
  print_string "===================\n";
  print_tests Lab8.simple_expr_eval_tests;
  print_tests Lab8.simple_func_eval_tests;
  print_tests Lab8.simple_call_eval_tests;
  print_tests Lab8.func_eval_tests;
  print_tests Lab8.call_eval_tests
;;

(* Main *)
prog_driver "lab8" eval_prog run_tests
