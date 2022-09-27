The purpose of this assignment is to understand the common
higher-order functions: map, filter, and fold.

Instructions
============

1. Merge the updated starter code into your team's repository.

2. Implement the following functions.  The approximate, expected
   number of lines of code for function are listed below.
   - `map` (~5 lines)
   - `filter` (~8 lines)
   - `fold_left` (~5 lines)
   - `fold_right` (~5 lines)
   - `append` using fold (~1 line)
   - `rev_append` using fold (~1 line)
   - `flatten` using fold (~1 line)
   - `insert` (~5 lines)
   - `insertionsort` using fold (~1 line)
   - `select` using fold (~6 lines)
   - `selectionsort` (~5 line)
   - `pivot` using fold (~8 lines)
   - `quicksort_simple` (~5 lines)
   - `quicksort_better` (~10 lines)

3. Add at least 5 new *non-trivial* unit tests per function, at the
   locations marked by `TODO` comments.

4. Test your code!

5. Push all above changes to your team repo in the github.com course
   organization.  Ensure that the code you want graded is in the
   master branch before the deadline.

Building and Testing
====================

- Type `make lab2` to build the lab
- Type `make lab2_test` to run the test cases defined in `src/lab2.ml`
- Each unit test is a tuple of the form `(optional_name, input,
  expected_output)`, where `optional_name` can either be `None` or
  `Some(x)`, where `x` is a human-readable name for the unit test. The
  `expected_output` is a `result` type, which allows you to use
  `Ok(out)` for regular output `out`, or `Error(ex)` if the unit test
  is expected to generate an exception `ex`.

Allowable OCaml Subset
======================

To help you learn to think in the functional programming style, this
project is restricted to a subset of OCaml.

You are permitted to use:
- binding (let)
- function definition (let, fun, let rec)
- function application (f x)
- pattern matching (match e with ...)
- conditionals (if e1 then e2 else e3)
- cons and list construction (a::d and [...])
- tuples ((...))
- arithmetic, Boolean, and equality operations (+,-,/,*,=, etc.)

You are NOT permitted to use:
- Loops (for, while)
- Mutable variables (ref)
- OCaml standard library or operator functions that directly implement
  the function you must implement.  For example, you may not implement
  map merely by using the OCaml List.map but must instead use
  recursion to implement the function yourself.

Grading
=======

| Item                                 | Points      |
|--------------------------------------|-------------|
| Student Tests are Present and Pass   | 30 (2/func) |
| Instructor Tests Pass                | 70 (5/func) |

