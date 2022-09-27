open Javascript_ast;;
open Javascript_main;;
open Testing;;

let eval_prog p =
  Printf.printf "result: %s\n"
    (str_value (Lab7.eval empty_env p))
;;

let run_tests () =
  print_string "Running Lab 7 Tests\n";
  print_string "===================\n";
  print_tests Lab7.simple_to_num_tests;
  print_tests Lab7.simple_to_bool_tests;
  print_tests Lab7.simple_to_str_tests;
  print_tests Lab7.simple_expr_eval_tests;
  print_tests Lab7.simple_print_eval_tests;
  print_tests Lab7.simple_cond_eval_tests;
  print_tests Lab7.simple_str_eval_tests;
  print_tests Lab7.simple_var_eval_tests;
  print_tests Lab7.cond_eval_tests;
  print_tests Lab7.str_eval_tests;
  print_tests Lab7.var_eval_tests;
;;

(* Main *)
prog_driver "lab7" eval_prog run_tests
