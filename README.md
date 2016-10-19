# Compiler Construction Development

master: [![Build Status](https://travis-ci.org/ChrisLane/Compiler-Construction.svg?branch=master)](https://travis-ci.org/ChrisLane/Compiler-Construction) develop: [![Build Status](https://travis-ci.org/ChrisLane/Compiler-Construction.svg?branch=develop)](https://travis-ci.org/ChrisLane/Compiler-Construction)

Implementing a compiler.

## Usage
### Build
Please install the following:
* Make
* Opam
* ocamlbuild (`$ opam install ocamlbuild`)
* ocamlfind (`$ opam install ocamlfind`)
* Menhir (`$ opam install menhir`)

Once these are installed, run the `make` command in the main directory. If successful, a binary should have been created in the same directory.

### Running
To the run the parser, use the command `./main <filename>`.
Test files can be found in the "tests" directory.
test/success source files will be parsed, test/fail source files will not be parsed

Example run: `./main test/success/bisection.src`
All tests can be done by running the `bash test/runtests.sh`. The script will output which tests passed and which failed.

## Syntax
 * Function (Function name, parameters, function body)
   * e.g. `functionName() {}`
   * Parameters are comma separated
   * Expressions in the body must all finish with a semicolon
 * Operators
   * All operators can take any expression on either or both sides (not only applies an expression from the right)
   * Addition `a + b`
   * Subtraction `a - b`
   * Multiplication `a * b`
   * Division `a / b`
   * Less than or equal to `a <= b`
   * Greater than or equal to `a >= b`
   * Equal to `a == b`
   * Not equal to `a != b`
   * And `a && b`
   * Or `a || b`
   * Not `!a`
 * While (while, condition, body)
   * e.g. `while (x>1) {}`
 * If (if, condition, body, else, body)
   * e.g. `if (x>1) {} else {}`
 * Assign (a = b)
   * e.g. `a = 11`
 * Return (return, expression to return)
   * e.g. `return x`
 * Application (f applied to x)
   * e.g. `f(x)`
 * Read int
   * e.g. `readint()`
 * Print int (printint, expression to print)
   * e.g. `printint(x)`
 * Let (let, name = expression, in body)
   * e.g. `let x = 10 {}`
 * New (type, name = expression)
   * e.g. `int x = 10`
