The purpose of this assignment is to help you understand algebraic
data types, persistence, and to further practice recursion and
higher-order functions.

Instructions
============

1. Do not re-clone this repository.  Merge the updated starter code
   into your team's repository as described in the main `README` file
   and Lab 0.

2. Implement the following functions.  The expected running time and
   approximate number of lines of code for each function are listed.
   - `map_inorder`: O(n), ~8 lines
   - `map_revorder`: O(n), ~8 lines
   - `is_bst`: O(n), ~12 lines
   - `bst_max`: O(height), ~5 lines
   - `bst_min`: O(height), ~5 lines
   - `bst_insert`: O(height), ~8 lines
   - `bst_search`: O(height), ~8 lines
   - `bst_remove_min`: O(height), ~10 lines
   - `bst_remove`: O(height), ~20 lines

3. Add at least 5 new *non-trivial* unit tests per function, at the
   locations marked by `TODO` comments.

4. Test your code!

5. Push all above changes to your team repo in the github.com course
   organization.  Ensure that the code you want graded is in the
   master branch before the deadline.

Building and Testing
====================

- Type `make lab3` to build the lab
- Type `make lab3_test` to run the test cases defined in `src/lab3.ml`
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
- options (Some X, None)

You are NOT permitted to use:
- Loops (for, while)
- Mutable variables (ref)
- OCaml libraries, operators, or functions that directly implement the
  functions you must implement.  For example, you may not use any
  existing tree or set libraries.

References
==========
- Lecture 07: Algebraic Data Types
- Lecture 08: Persistence
- [Clarkson. Ch 8.3.1: Binary Search Trees]
  (https://cs3110.github.io/textbook/chapters/ds/rb.html)

 
Grading
=======

| item                                 | points      |
|--------------------------------------|-------------|
| student tests are present and pass   | 50 (5/func) |
| instructor tests pass                | 50 (5/func) |

