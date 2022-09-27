open Testing
open Lab4

let main () =
  print_string "Running Lab 4\n";
  print_string "=============\n";
  Testing.print_tests Lab4.rbt_is_invariant_tests;
  Testing.print_tests Lab4.rbt_is_sorted_tests;
  Testing.print_tests Lab4.rbt_search_tests;
  Testing.print_tests Lab4.rbt_balance_tests;
  Testing.print_tests Lab4.rbt_insert_tests;
;;

main ()
