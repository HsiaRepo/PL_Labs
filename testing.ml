
(* A test case consists of:
 *   - An optional name (e.g., Some(name) or None)
 *   - A tuple of the arguments to the function to test
 *   - The expected output (e.g, Ok y) or exception (e.g., Error y)
 *     of the function to test.
 *
 * A test group consists of:
 *   - An optional name (e.g., Some(name) or None)
 *   - The function to test.
 *   - function for equality of output
 *   - function for equality of exceptions
 *   - pretty printers for input and output
 *   - list of test cases
 *)

let test_counts = ref (0,0) (* total, failed *)

let run_test (
  (f : ('a -> 'b)), (* function to test *)
  (input : 'a)      (* provided input *)
) : ('b,exn) result =
  (*flush stdout;
  Unix.sleepf 0.5;*)
  (try Ok(f input) with ex -> Error(ex))

let list_check (
  (name : string), (* name of test group *)
  (f : ('a -> 'b)), (* function to test *)
  (eq : ('b -> 'b -> bool)), (* equality of function output *)
  (eq_exn : (exn -> exn -> bool)), (* equality of exceptions *)
  (pp : (('a -> string) * ('b -> string)) option), (* pretty printers for input and output *)
  (l : (string option * 'a * ('b, exn) result) list) (* list of tests *)
  ) = (
  Printf.printf "running \"%s\" tests...\n" name;
  flush stdout;
  let failed = List.rev (fst (List.fold_left (fun (acc,count) (name, a,b) ->
    let name = (match name with
    | None -> string_of_int count
    | Some(s) -> Printf.sprintf "\"%s\"" s) in
    let result = run_test (f,a) in
    let (str_in,str_out) = (match pp with
    | Some(si,so) -> ((fun x -> Some(si x)), (fun x -> Some(so x)))
    | None -> ((fun x -> None), (fun x -> None))) in
    let (success,str1,str2) = (match (result,b) with
    | (Ok(x),Ok(y)) ->       (eq x y, str_out x, str_out y)
    | (Error(x),Error(y)) -> (eq_exn x y, Some(Printexc.to_string x), Some(Printexc.to_string y))
    | (Ok(x),Error(y)) ->    (false, str_out x, Some(Printexc.to_string y))
    | (Error(x),Ok(y)) ->    (false, Some(Printexc.to_string x), str_out y)
    ) in
    let str_opt o = (match o with
    | Some(s) -> s
    | None -> "?") in
    ((if success then (
      Printf.printf "  pass test %s\n" name;
      acc
    ) else (
      Printf.printf "  FAIL test %s\n" name;
      (match pp with
      | Some(str_input,str_output) -> Printf.printf "    input: %s\n    output: %s\n    expected: %s\n" (str_input a) (str_opt str1) (str_opt str2)
      | None -> ());
      ((result,b)::acc)
    )), count+1)
    ) ([],1) l)) in
    let (total,failed) = (List.length l, List.length failed) in
    let (old_total,old_failed) = !test_counts in
    test_counts := (old_total+total, old_failed+failed);
    (total,failed)
  )

let run_tests (name,f,eq,eq_exn,pp,l) =
  list_check (name,f,eq,eq_exn,pp,l)

let print_tests (name,f,eq,eq_exn,pp,l) =
  let _ = list_check (name,f,eq,eq_exn,pp,l) in
  ()

let get_test_counts (curr_total,curr_failed) =
  let (total,failed) = !test_counts in
  (curr_total+total, curr_failed+failed)

let reset_test_counts () =
  test_counts := (0,0)
