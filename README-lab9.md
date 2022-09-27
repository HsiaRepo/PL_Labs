The purpose of this assignment is to understand state mutation and
references.

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
functionality described in `src/lab09.ml`.  Skeleton code is provided,
and you will need to fill in the body of several functions. Each
location where you need to replace a placeholder expression with your
own code is marked with a `TODO` comment.

In this lab, we will use the skills developed in the prior labs to
continue building our interpreter for a simple subset of
JavaScript. The grammar used in this lab is shown below. Beyond the
previous JavaScript subset, we are now adding support for
*objects/references* and *mutable variables*.

Our `environment` data structure from the previous lab has been
extended to additionally track the contents of the *heap*.

You can check your work by comparing your evaluator's output to that of an
existing JavaScript interpreter such as `nodejs`.

- **program** *p* ::= *e* | `const` *x* `=` *e* `;` *p* | `let` *x* `=` *e* `;` *p* | *x* `=` *e* `;` *p*

- **block** *bl* ::= `return` *e* | `const` *x* `=` *e* `;` *bl* | `let` *x* `=` *e* `;` *bl* | *e* `=` *e* `;` *bl*

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

Notice that the sigature of `eval` and related functions has changed
to now pass and return a heap.  Update your evaluator from the
previous lab to match the new function signature.  You must pass the
heap to all recursive calls and use the updated heap returned by such
calls.

Task 2
------

Add support for *mutable variables*. In `eval_stmt`, you will need to
additionally handle the `LetStmt`, in addition to the `ConstStmt` we
handled in the previous labs. As before, this causes a new binding for
a variable to be placed in the environment, but in this case, marks it
as `Mutable` instead of `Immutable` and allocates a reference cell.
See the `heap_alloc` function in `javascript_heap.ml`.

Additionally, you will need to handle the `AssignStmt`, which denotes
an update to a mutable variable. In this case, you should look up the
variable in the environment to make sure it is indeed mutable, and if
so, update its contents in the heap. See the `update_environment`
function in `javascript_ast.ml` and the `heap_assign` function in
`javascript_heap.ml`.

Finally, you will need to handle cases where a `VarExpr` refers to
mutable variables (stored as references into the heap).  See the
`head_deref` function in `javascript_heap.ml`

Add at least 5 unit tests for this functionality to the
`mut_eval_tests` list (location marked with `TODO`).


Documentation
-------------

Please provide concise documentation (comments in the code) for each
of the features you implement.
