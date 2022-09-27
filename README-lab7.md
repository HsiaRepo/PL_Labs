The purpose of this assignment is to extend your understanding of
*evaluation* to include features such as strings, conditionals,
output, constant variables, and automatic type conversion.

Instructions
============

1. Merge the updated starter code into your team's repository.

      git pull upstream

2. Complete the tasks below.

3. Test your code!

4. Push all above changes to your team repo in the github.com course
   organization.  Ensure that the code you want graded is in the
   master branch before the deadline.

Your goal in this lab will be to add OCaml code to complete the
functionality described in `src/lab7.ml`.  Skeleton code is provided,
and you will need to fill in the body of several functions. Each
location where you need to replace a placeholder expression with your
own code is marked with a `TODO` comment.

In this lab, we will use the skills developed in Lab 7 to continue
building an interpreter for a simple subset of JavaScript. The
following is the grammar for the Lab 3 JavaScript subset. Notice that
we will now add support for *immutable variables*, *strings*,
*conditionals*, and a *console print operator*.  Notice also that the
JavaScript automatic type conversions must be performed, e.g., the
program `"one" + 2` should evaluate to `"one2"`.

You can check your work by comparing your evaluator's output to that
of an existing JavaScript interpreter such as `nodejs` (installed as
`node` on some systems).

Here is the grammar used for this project (note that this is a subset
of JavaScript).

- **program** *p* ::= *e* | `const` *x* `=` *e* `;` *p*

- **expression** *e* ::= *x* | *v* | *uop* *e* | *e* *bop* *e*
                | *e* `?` *e* `:` *e* | `console.log` `(` *e* `)`

- **value** *v* ::= *n* | *b* | *s* | `undefined`

- **unary operator** *uop* ::= `-` | `!`

- **binary operator** *bop* ::= `+` | `-` | `*` | `/` | `===` | `!==` | `<` | `<=` | `>` | `>=` | `&&` | `||`

- **identifier** *x*

- **number (float)** *n*

- **boolean** *b* ::= `true` | `false`

- **string** *s*

Task 1
------

Add support for the console print operator. Evaluating a program such
as `console.log(e)` should evaluate expression `e`, and print the
resulting value.

Secondly, add support for conditional expressions. This is
JavaScript's "inline *if*".  Add at least 3 unit tests for this
conditional functionality to the `cond_eval_tests` list (location
marked with `TODO`).

Task 2
------

Add support for strings. Note that the `+`, `<`, `<=`, `>`, `>=`
operators work differently for strings versus numbers.  Add at least 3
unit tests for your string functionality to the `str_eval_tests` list
(location marked with `TODO`).

Task 3
------

Add support for immutable (`const`) variables. The comments at the
beginning of `lab7.ml` point you to some functions that will be
useful in manipulating the environment to read/push variable
bindings. Note that in our interpreter, we will allow "redefinition"
of variables, e.g. consider `const x = 1; const x = 2; x` to be a
valid program, which evaluates to the value `2`.

Add at least 3 unit tests for your variable-related functionality to
the `var_eval_tests` list (location marked with `TODO`).


Documentation
--------------

- Please provide concise documentation for each of the features you implement.

Using Your Interpreter from the Command Line
--------------------------------------------

- Type `./lab7` to read from standard input.  If you are reading from
  the terminal, press CTRL-D when finished entering the expression.
- You can also pipe input into the program, e.g. `echo '1+2' |
  ./lab7`.
- Type `./lab7 file.js` to read input from a file name file.js.

References
==========

- JavaScript:
  - [JavaScript Standard](https://262.ecma-international.org/10.0/)
  - Mozilla's [JavaScript Guide](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide)
  - [NodeJS](https://nodejs.org/) JavaScript Runtime
