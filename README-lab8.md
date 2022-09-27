The purpose of this assignment is to extend your understanding of
*evaluation* to include function definition, function application, and
lexical closures.

Instructions
============

1. Merge the updated starter code into your team's repository.  (Use
   `git pull` or `git merge` from your team repository; do not copy
   files manually).

       git pull upstream

2. Complete the tasks below.

3. Test your code!

4. Push all above changes to your team repo in the github.com course
   organization.  Ensure that the code you want graded is in the
   master branch before the deadline.

Your goal in this lab will be to add OCaml code to complete the
functionality described in `src/lab8.ml`.  Skeleton code is provided,
and you will need to fill in the body of several functions. Each
location where you need to replace a placeholder expression with your
own code is marked with a `TODO` comment.

In this lab, we will use the skills developed in the prior labs to
continue building a "big-step" interpreter for a simple subset of
JavaScript. The following is the grammar for this lab's JavaScript
subset. Notice that we will now add support for *recursive
higher-order functions* and *function calls*.  You must also properly
support *static scope* using lexical closures.

You can check your work by comparing your evaluator's output to that
of an existing JavaScript interpreter such as `nodejs` (installed as
`node` on some systems).

- **program** *p* ::= *e* | `const` *x* `=` *e* `;` *p*

- **block** *bl* ::= `return` *e* | `const` *x* `=` *e* `;` *bl*

- **expression** *e* ::= *x* | *v* | *uop* *e* | *e* *bop* *e*
                | *e* `?` *e* `:` *e* | `console.log` `(` *e* `)` | *e* `(` *e* `)`

- **value** *v* ::= *n* | *b* | *s* | `undefined` | `function` *x* `(` *x* `)` `{` *bl* `}`

- **unary operator** *uop* ::= `-` | `!`

- **binary operator** *bop* ::= `+` | `-` | `*` | `/` | `===` | `!==` | `<` | `<=` | `>` | `>=` | `&&` | `||`

- **identifier** *x*

- **number (float)** *n*

- **boolean** *b* ::= `true` | `false`

- **string** *s*

Task 1
------

Add support for function definitions. Evaluating a program such as
```
const f = function(x){ return x+1 };
console.log("Function: "+f)
```
should evaluate the program, and pretty-print the function.

We also need to capture information about the environment in which the
function was defined.  Evaluating a program such as `const x = 123;
function(y){ return x+y }` should produce a `ClosureVal` in which the
map contains a single binding for the variable `x`, which is the only
variable present in the function's define-time environment.

Add at least 2 unit tests for this functionality to the
`func_eval_tests` list (location marked with `TODO`).

Task 2
------

Add support for function calls. For example, evaluating the following
```
const f = function(x){ return x+1; };
const r = f(2);
r+3
```
should result in the value `6`.

Ensure your function calls use lexical scope. For example, evaluating the following:
```
const x = 5;
const f = function(y){ return x + y };
(function(z) { const x = 7; return f(6) })(0)
```
should result in the value `11`.

This involves using the *closure* to obtain the correct environment in
which to evaluate the function's body.

Note: you can use the Node.js JavaScript interpreter to check the
results of your interpreter (this is the `nodejs` or `node` command on
Linux).

Add at least 5 unit tests for your function-call functionality to the
`call_eval_tests` list (location marked with `TODO`).

Documentation
-------------

Please provide concise documentation (comments in the code) for each
of the features you implement.

Using Your Interpreter from the Command Line
--------------------------------------------

- Type `./lab8` to read from standard input.  If you are reading from
  the terminal, press CTRL-D when finished entering the expression.
- You can also pipe input into the program, e.g. `echo '1+2' |
  ./lab8`.
- Type `./lab8 file.js` to read input from a file name file.js.
- Type `./lab8 --test` to run the test cases.


References
==========

- JavaScript:
  - [JavaScript Standard](https://262.ecma-international.org/10.0/)
  - Mozilla's [JavaScript Guide](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide)
  - [NodeJS](https://nodejs.org/) JavaScript Runtime
