# Compiler Construction Development

master: [![Build Status](https://travis-ci.org/ChrisLane/custom-lang-compiler.svg?branch=master)](https://travis-ci.org/ChrisLane/custom-lang-compiler) dev: [![Build Status](https://travis-ci.org/ChrisLane/custom-lang-compiler.svg?branch=dev)](https://travis-ci.org/ChrisLane/custom-lang-compiler)

Implementing a compiler.

## Usage
### Build
Please install the following:
* Make
* Ocaml
* Opam
* Ocamlbuild (`$ opam install ocamlbuild`)
* Ocamlfind (`$ opam install ocamlfind`)
* Menhir (`$ opam install menhir`)

Once these are installed, run the `make` command in the main directory. If successful, a binary should have been created in the same directory.

### Running Tasks
(./main \<task> \<filename>)
Running the ./main binary on its own will output available tasks.

Available Flags:
 * -o -         Optimise the parsed program.

Available Tasks:
 * parse -      This will parse the source file and output the parsed structure.
 * evaluate -   This will evaluate the source file and return an output for the function.
 * interpret -  This will interpret the source file as assembly.
 * codegen -    This will generate assembly-like code.
 * codegenx86 - This will generate x86 assembly code.

Example run: `./main parse mysourcefile.src`

### Testing
The status of the tests included in the repository can be found at the [Travis Page](https://travis-ci.org/ChrisLane/Compiler-Construction), this will display "Passed" if all tests are passing and "Failed" if a test fails.

Test sources can be found in the "tests" directory.
"success" directory files will succeed, "fail" directory files will purposefully fail.
Source files use the .src extension and must have a matching filename with extension .out in the "output" directory that contains the expected output for the source file.

Example run: `./main parse test/parse/success/bisection.src`

All tests can be done by running the `make test` command. The script will output which tests passed and which failed.

## Syntax
 * Function (Function name, parameters, function body)
   * e.g. `functionName() {}`
   * Parameters are comma separated
   * Expressions in the body must all finish with a semicolon
 * Operators
   * All operators can take any expression on either or both sides (not only applies to an expression on the right). x and y represent the integer type for evaluation, a and b represent the boolean type.
   * Addition `x + y`
   * Subtraction `x - y`
   * Multiplication `x * y`
   * Division `x / y`
   * Less than or equal to `x <= y`
   * Greater than or equal to `x >= y`
   * Less than `x < y`
   * Greater than `x > y`
   * Equal to `x == y`
   * Not equal to `x != y`
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
 * Application (f with argument x)
   * e.g. `f(x)`
 * Readint
   * e.g. `readint()`
 * Break from a loop
   * e.g. `break`
 * Continue to next loop iteration
   * e.g. `continue`
 * Print (print, expression to print)
   * e.g. `print x`
 * Let (let, name = expression, in body)
   * e.g. `let x = 10 {}`
 * New (var, name = expression)
   * e.g. `var x = 10`
 * Deref
   * e.g. `$variable`

## Types
Supported types are listed:
 * Integer - Whole numbers
 * Boolean - true or false
 * Identifier - String identifier for storing expressions.
 * Unit - Empty

## Expression Return Types
 * Body Section - Returns the last type evaluated and any printed output.
 * Function - Returns the body section.
 * Operators
   * +, -, *, / - Returns an integer.
   * <=, >=, <, >, ==, || - Returns a boolean.
   * &&, || - Returns a boolean.
   * ! - Returns a boolean.
 * While - Returns a Unit type.
 * If - Returns the first body section if the condition is true, else it returns the second body section.
 * Assign - Returns the type of the value assigned.
 * Return - Returns the type of the argument expression.
 * Print - Returns a Unit type.
 * Deref - Returns an integer.
 * Application - Returns the result of a function.
 * Readint - Returns an integer.
 * Let - Returns a Unit type.
 * New - Returns a Unit type.
