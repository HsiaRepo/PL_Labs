The purpose of this assignment is to understand *lexing*, *parsing*
and how to use a *lexer generator* and *parser generator*.


Instructions
============

1. Merge the updated starter code into your team's repository.

2. Implement (add or fix) the lexing rules in `javascript_lexer.mll`
   in the places marked with TODO comments.

3. Implement (add or fix) the parsing rules in
   `src/javascript_parser.mly` in the places marked with TODO
   comments.

4. In `lab5_tests.ml`, add at least 10 new *non-trivial* unit tests
   for the lexer and 10 new *non-trivial* unit tests for the parser.

5. Test your code!

6. Push all above changes to your team repo in the github.com course
   organization. Ensure that the code you want graded is in the master
   branch before the deadline.


Building and Testing
====================

- Type `make lab5` to build the lab
- Type `make lab5_test` to run the test cases defined in
  `src/lab5_tests.ml`
- Each unit test is a tuple of the form `(optional_name, input,
  expected_output)`, where `optional_name` can either be `None` or
  `Some(x)`, where `x` is a human-readable name for the unit test. The
  `expected_output` is a `result` type, which allows you to use
  `Ok(out)` for regular output `out`, or `Error(ex)` if the unit test
  is expected to generate an exception `ex`.

Grading
=======

| item                                 | points      |
|--------------------------------------|-------------|
| student tests are present and pass   | 50 (25/func)|
| instructor tests pass                | 50 (25/func)|

Using Your Lexer and Parser from the Command Line
-------------------------------------------------

The lab5 main function can also read JavaScript from files and
standard input. After reading the entire input, the program will print
a list of the tokens or the abstract syntax.

- Type `./lab5 --lex` or `./lab5 --parse` to read from standard
  input. If you are reading from the terminal, press CTRL-D when
  finished entering the expression.
- You can also pipe input into the program, e.g.,
  - `echo "1+2" | ./lab5 --lex`.
  - `echo "1+2" | ./lab5 --parse`.
- Type `./lab5 --lex file.js` or `./lab5 --parse file.js` to read
  input from a file named `file.js`.


References
==========

- Lexing:
  - Lecture 09: Syntax
  - Lecture 10: Lexing
  - ocamllex and ocamlyacc [manual](https://ocaml.org/manual/lexyacc.html)
- JavaScript:
  - [JavaScript Standard](https://262.ecma-international.org/10.0/)
  - Mozilla's [JavaScript Guide](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide)
  - [NodeJS](https://nodejs.org/) JavaScript Runtime
