open Ast
open Assembly
open Buffer

let code = create 25
let sp = ref 0
let addr_base = ref 0

(* Generate a string for the int value of a bool *)
let string_int_of_bool n = if n then string_of_int 1 else string_of_int 0

(* The string for instructions of operators *)
let instruction_of_op = function
  | Plus    -> "\taddq\t%rbx, %rax\n"
  | Minus   -> "\tsubq\t%rbx, %rax\n"
  | Times   -> "\timulq\t%rbx\n"
  | Divide  -> "\tmovq\t$0, %rdx\n" ^ "\tidivq\t%rbx\n"
  | Leq     -> "\tcmpq\t%rbx, %rax\n" ^ "\tsetle\t%al\n" ^ "\tmovzbq\t%al, %rax\n"
  | Geq     -> "\tcmpq\t%rbx, %rax\n" ^ "\tsetge\t%al\n" ^ "\tmovzbq\t%al, %rax\n"
  | Equal   -> "\tcmpq\t%rbx, %rax\n" ^ "\tsete\t%al\n" ^ "\tmovzbq\t%al, %rax\n"
  | Noteq   -> "\tcmpq\t%rbx, %rax\n" ^ "\tsetne\t%al\n" ^ "\tmovzbq\t%al, %rax\n"
  | And     -> "\tandq\t%rbx, %rax\n"
  | Or      -> "\torq\t%rbx, %rax\n"
  | _       -> failwith "Operator not implemented."

(* Instructions for a const *)
let codegenx86_const n =
  "\tpushq\t$" ^ (string_of_int n) ^ "\n"
  |> add_string code

(* Instructions for a bool*)
let codegenx86_bool n =
  "\tpushq\t$" ^ (string_int_of_bool n) ^ "\n"
  |> add_string code

(* Instructions for an operator *)
let codegenx86_op op =
  "\tpopq\t%rbx\n" ^
  "\tpopq\t%rax\n" ^
  (instruction_of_op op) ^
  "\tpushq\t%rax\n"
  |> add_string code

(* Instructions for an identifier *)
let codegenx86_id addr =
  "\t//offset\t" ^ (string_of_int addr) ^ "\n" ^
  "\tmov\t" ^ (-16 -8 * addr |> string_of_int) ^ "(%rbp), %rax\n" ^
  "\tpush\t%rax\n"
  |> add_string code

(* Instructions for a let *)
let codegenx86_let _ =
  "\tpopq\t%rax\n" ^
  "\tpopq\t%rbx\n" ^
  "\tpushq\t%rax\n"
  |> add_string code

(* Instructions for a new variable *)
let codegenx86_new _ =
  "\tpopq\t%rax\n" ^
  "\tpopq\t%rbx\n" ^
  "\tpushq\t%rax\n"
  |> add_string code

(* Instructions for creating a pointer *)
let codegenx86_ptr _ =
  "\tleaq\t" ^ (-16 -8 * !sp |> string_of_int) ^ "(%rbp), %rax\n" ^
  "\tpushq\t%rax\n"
  |> add_string code

(* Instructions for dereferencing *)
let codegenx86_deref _ =
  "\tpopq\t%rbx\n" ^
  "\tmovq\t(%rbx), %rax\n" ^
  "\tpushq\t%rax\n"
  |> add_string code

(* Instructions for assignment *)
let codegenx86_asg _ =
  "\tpopq\t%rbx\n" ^
  "\tpopq\t%rax\n" ^
  "\tmovq\t%rbx, (%rax)\n" ^
  "\tpushq\t%rbx\n"
  |> add_string code

(* Instructions for dereferencing *)
let codegenx86_deref _ =
  "\tpopq\t%rbx\n" ^
  "\tmovq\t(%rbx), %rax\n" ^
  "\tpushq\t%rax\n"
  |> add_string code

(* Instructions for print *)
let codegenx86_print _ =
  "\tpopq\t%rdi\n" ^
  "\tcall\tprint\n"
  |> add_string code

(* Lookup the address for a value *)
let rec lookup x = function
  | [] -> failwith "Could not find symbol address."
  | (y, addr)::ys when x = y -> addr
  | _::ys -> lookup x ys

(* Generate x86 code for expressions *)
let rec codegenx86 symt = function
  | Empty -> "\tnop\n" |> add_string code
  | Operator (op, e1, e2) ->
    codegenx86 symt e1;
    codegenx86 symt e2;
    codegenx86_op op;
    sp := !sp - 1
  | Identifier x ->
    let addr = lookup x symt in
    codegenx86_id (addr);
    sp := !sp + 1
  | Const n ->
    codegenx86_const n;
    sp := !sp + 1
  | Bool n ->
    codegenx86_bool n;
    sp := !sp + 1
  | Let (x, e1, e2) ->
    codegenx86 symt e1;
    codegenx86 ((x, !sp) :: symt) e2;
    codegenx86_let ();
    sp := !sp - 1;
  | New (x, e1, e2) ->
    codegenx86 symt e1;
    codegenx86_ptr ();
    sp := !sp + 1;
    codegenx86 ((x, !sp) :: symt) e2;
    codegenx86_new ();
  | Seq (e, Empty) -> codegenx86 symt e
  | Seq (e1, e2) ->
    let _ = codegenx86 symt e1 in
    codegenx86 symt e2
  | Asg (e1, e2) ->
    codegenx86 symt e1;
    codegenx86 symt e2;
    codegenx86_asg ();
    sp := !sp + 1
  | Deref n ->
    codegenx86 symt n;
    codegenx86_deref ()
  | Print n ->
    codegenx86 symt n;
    codegenx86_print ()
  | _ -> failwith "Unimplemented expression."

(* Generate x86 code for a function *)
let codegenx86_func func =
  reset code;
  add_string code codegenx86_prefix;
  addr_base := 0;
  codegenx86 [] func;
  add_string code codegenx86_suffix;
  output_buffer stdout code;
  ""

(* Generate x86 code for a program *)
let rec codegenx86_prog = function
  | []                              -> failwith "Codegenx86 requires a 'main' function."
  | Fundef ("main", args, body)::ys -> codegenx86_func body
  | _::ys                           -> codegenx86_prog ys
