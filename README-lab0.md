The purpose of this assignment is to ensure that you are able to use
Git and OCaml.

Instructions
============

1. Clone this repo as described in the main README.

2. Edit the `.gitignore` file to ignore object files (`*.o`), OCaml
   interface files (`*.cmi`), OCaml native object files (`*.cmx`), the
   executable for this lab (`lab0`), and temporary or backup files that
   your editor may generate.

3. In `src/lab0.ml`, modify the code to print "Hello, World!"

4. Use `make` to compile `lab0`.  Ensure that each team member is able
   to edit, compile, and run the program.

5. Push all above changes to your team repo in the github.com course
   organization.

6. Each team member must submit via Canvas a PDF containing the
following:
   1. The git command(s) for cloning the starter code repository.
   2. The git command(s) for the "origin" remote be your team's
      repository.
   3. The git command(s) for the "upstream" remote be the starter
      code.
   4. The git command(s) to merge updates to the starter code.
   5. A screenshot on their own system or account showing the URL for
      the origin remote: (`git remote get-url origin`)
   6. A screenshot on their own system or account showing the URL for
      the upstream remote: (`git remote get-url upstream`)
   7. A screenshot on their own system or account showing the
      compilation and execution of the "Hello World" program.

Grading
=======

| Item                            | Points |
|---------------------------------|--------|
| Correctly-named Repo            | 3      |
| Private Repo                    | 1      |
| AUTHORS file                    | 1      |
| .gitignore / no cruft in repo   | 2      |
| "Hello World" code              | 3      |
| Correct name & email in commits | 2      |
| Clone Command                   | 1      |
| Origin Command                  | 1      |
| Upstream Command                | 1      |
| Merge Command                   | 1      |
| Origin Screenshot               | 2      |
| Upstream Screenshot             | 2      |
| "Hello World" Screenshot        | 5      |
