open Testing
open Lab1

let main () =
  Testing.print_tests Lab1.nth_tests;
  Testing.print_tests Lab1.append_tests;
  Testing.print_tests Lab1.reverse_tests;
  Testing.print_tests Lab1.length_tests;
  Testing.print_tests Lab1.list_prefix_tests;
  Testing.print_tests Lab1.list_suffix_tests;
  Testing.print_tests Lab1.merge_tests;
  Testing.print_tests Lab1.mergesort_tests;
;;

main ()
