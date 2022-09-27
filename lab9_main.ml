open Javascript_ast
open Javascript_heap
open Javascript_main
open Testing

let eval_prog p =
  let v,h = Lab9.eval empty_env empty_heap p in
  Printf.printf "result: %s\n%s\n"
    (str_value v)
    (str_heap h)
;;

let run_tests () =
  print_string "Running Lab 9 Tests\n";
  print_string "===================\n";
  print_tests Lab9.simple_expr_eval_tests;
  print_tests Lab9.simple_func_eval_tests;
  print_tests Lab9.simple_call_eval_tests;
  print_tests Lab9.simple_mut_eval_tests;
  print_tests Lab9.mut_eval_tests
;;

(* Main *)
prog_driver "lab9" eval_prog run_tests
