The purpose of this assignment is to introduce *evaluation* and
*type-checking*

Instructions
============

Your goal in this lab will be to add OCaml code to complete the
functionality described in `src/lab6.ml`.  Skeleton code is provided,
and you will need to fill in the body of several functions. Each
location where you need to replace a placeholder expression with your
own code is marked with a `TODO` comment.

1. Merge the updated starter code into your team's repository.

2. Complete thee tasks below.

3. Test your code!

4. Push all above changes to your team repo in the github.com course
   organization.  Ensure that the code you want graded is in the
   master branch before the deadline.

Task 1
------

In this part of the lab, we will begin building our JavaScript
interpreter.  We will start by building a function which evaluates
simple JavaScript *expressions*.  For example, when given a JavaScript
expression such as `1+1`, your code should return the value `2`.

For now, we will only consider *well-typed* expressions, such as the
above.  In this lab, expressions such as `true + 1` or `100 && false`
will be considered to be *malformed*, both in our expression
evaluator, and in our type checker (Task 2), and your evaluator should
return `UndefVal` for such expressions.  See the instructor-provided
test cases for several examples.

Here is the grammar for expressions (note that this is a subset of
JavaScript).

- **expression** *e* ::= *v* | *uop* *e* | *e* *bop* *e*

- **value** *v* ::= *n* | *b*

- **unary operator** *uop* ::= `-` | `!`

- **binary operator** *bop* ::= `+` | `-` | `*` | `/` | `===` | `!==` | `<` | `<=` | `>` | `>=` | `&&` | `||`

- **number (float)** *n*

- **boolean** *b* ::= `true` | `false`

- Edit `lab6.ml` at the locations indicated by `TODO` comments, and
  complete the `eval` function as described.
- Add at least 5 new *non-trivial* unit tests for this function, at
  the location indicated by the `TODO` comment.

Task 2
------

Beyond evaluating expressions, you will also implement a basic
bottom-up typechecker for expressions. For example, when given a
JavaScript expression such as `1 + (2 + 3)`, your typechecker should
indicate that this expression is of type *number*.

The types needed for the above expressions are `number` and `boolean`.

A typechecking failure (e.g., a malformed input expression, as
described in Task 1) is indicated by returning `None`.

- Edit `lab6.ml` at the locations indicated by `TODO` comments, and
  complete the `typecheck` function as described.
- Add at least 5 new *non-trivial* unit tests for this function, at
  the location indicated by the `TODO` comment.

Documentation
--------------

- Please provide concise documentation for each of the features you implement.

Using Your Interpreter from the Command Line
--------------------------------------------

- Type `./lab6` to read from standard input.  If you are reading from
  the terminal, press CTRL-D when finished entering the expression.
- You can also pipe input into the program, e.g. `echo '1+2' |
  ./lab6`.
- Type `./lab6 file.js` to read input from a file name file.js.

References
==========

- JavaScript:
  - [JavaScript Standard](https://262.ecma-international.org/10.0/)
  - Mozilla's [JavaScript Guide](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide)
  - [NodeJS](https://nodejs.org/) JavaScript Runtime
