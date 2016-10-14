# Compiler-Construction
Implementing a compiler.

## Build
Please install the following:
* Make
* Opam
* Menhir ($ opam install menhir)

Once these are installed, run the `make` command in the main directory. If successful, a binary should have been created in the same directory.

## Running
To the run the parser, use the command `./main <filename>`.
Test files can be found in the "tests" directory. 
test/success source files will be parsed, test/fail source files will not be parsed

example run: `./main test/success/bisection.src`


