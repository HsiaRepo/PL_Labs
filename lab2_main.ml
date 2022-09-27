open Testing
open Lab2

let main () =
  (* Implementation *)
  Testing.print_tests Lab2.map_tests;
  Testing.print_tests Lab2.filter_tests;
  Testing.print_tests Lab2.fold_left_tests;
  Testing.print_tests Lab2.fold_right_tests;

  (* Use *)
  
  Testing.print_tests Lab2.append_tests;
  Testing.print_tests Lab2.rev_append_tests;
  Testing.print_tests Lab2.flatten_tests;

  Testing.print_tests Lab2.insert_tests;  
  
  Testing.print_tests Lab2.insertionsort_tests;
  
  Testing.print_tests Lab2.select_tests;
  Testing.print_tests Lab2.selectionsort_tests;
  
  Testing.print_tests Lab2.pivot_tests;
  
  Testing.print_tests Lab2.quicksort_simple_tests;
  (*
  Testing.print_tests Lab2.quicksort_better_tests;
  *)
;;

main ()
