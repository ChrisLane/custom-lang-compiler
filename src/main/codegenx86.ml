open Ast
open Assembly
open Buffer

let code = create 25
let sp = ref 0
let lblp = ref 0

(* Generate a string for the int value of a bool *)
let string_int_of_bool n = if n then string_of_int 1 else string_of_int 0

(* The string for instructions of operators *)
let instruction_of_op = function
  | Plus    -> "\taddq\t%rbx, %rax\n"
  | Minus   -> "\tsubq\t%rbx, %rax\n"
  | Times   -> "\timulq\t%rbx\n"
  | Divide  -> "\txorq\t%rdx, %rdx\n" ^ "\tidivq\t%rbx\n"
  | Leq     -> "\tcmpq\t%rbx, %rax\n" ^ "\tsetle\t%al\n" ^ "\tmovzbq\t%al, %rax\n"
  | Geq     -> "\tcmpq\t%rbx, %rax\n" ^ "\tsetge\t%al\n" ^ "\tmovzbq\t%al, %rax\n"
  | Equal   -> "\tcmpq\t%rbx, %rax\n" ^ "\tsete\t%al\n" ^ "\tmovzbq\t%al, %rax\n"
  | Noteq   -> "\tcmpq\t%rbx, %rax\n" ^ "\tsetne\t%al\n" ^ "\tmovzbq\t%al, %rax\n"
  | And     -> "\tandq\t%rbx, %rax\n"
  | Or      -> "\torq\t%rbx, %rax\n"
  | Not     -> "\tcmpq\t%rbx, %rax\n" ^ "\tsetz\t%al\n" ^ "\tmovzbq\t%al, %rax\n"

(* Instructions for empty *)
let codegenx86_empty _ =
  "\tpushq\t$0\n"
  |> add_string code

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
  "\tmovq\t" ^ (-16 -8 * addr |> string_of_int) ^ "(%rbp), %rax\n" ^
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
  "\tpopq\t%rax\n" ^
  "\tpopq\t%rbx\n" ^
  "\tmovq\t%rax, (%rbx)\n" ^
  "\tpushq\t%rax\n"
  |> add_string code

(* Instructions for dereferencing *)
let codegenx86_deref _ =
  "\tpopq\t%rbx\n" ^
  "\tmovq\t(%rbx), %rax\n" ^
  "\tpushq\t%rax\n"
  |> add_string code

(* Instruction to pop from the stack *)
let codegenx86_pop _ =
  "\tpopq\t%rax\n"
  |> add_string code

(* Instructions for print *)
let codegenx86_print _ =
  "\tpopq\t%rdi\n" ^
  "\tcall\tprint\n"
  |> add_string code

(* Instructions to test and jump if gate zero *)
let codegenx86_testjz _ =
  "\tpopq\t%rax\n" ^
  "\ttest\t%rax, %rax\n" ^
  "\tjz .L" ^ (string_of_int !lblp) ^ "\n"
  |> add_string code

(* Instructions to test and jump if gate not zero *)
let codegenx86_testjnz _ =
  "\tpopq\t%rax\n" ^
  "\ttest\t%rax, %rax\n" ^
  "\tjnz .L" ^ (string_of_int !lblp) ^ "\n"
  |> add_string code

(* Generate a label *)
let codegenx86_lbl n =
  ".L" ^ (string_of_int n) ^ ":\n"
  |> add_string code

(* Instruction to jump to a label and create a new label*)
let codegenx86_jmplbl x y =
  "\tjmp .L" ^ (string_of_int x) ^ "\n"
  |> add_string code;
  codegenx86_lbl y

(* Instructions that come before a function definition *)
let codegenx86_newfunc n =
  "\t.globl\t" ^ n ^ "\n" ^
  "\t.type\t" ^ n ^ ", @function\n" ^
  n ^ ":\n" ^
  "\tpushq\t%rbp\n" ^
  "\tmovq\t%rsp, %rbp\n" ^
  "\tsubq\t$16, %rsp\n"
  |> add_string code

(* Instructions for the end of a function *)
let codegenx86_endfunc _ =
  "\tpopq\t%rax\n" ^
  "\tleave\n" ^
  "\tret\n"
  |> add_string code

(* Instructions to call a function and store it's value *)
let codegenx86_call n =
  "\tcall\t" ^ n ^ "\n" ^
  "\tpushq\t%rax\n"
  |> add_string code

(* A function to get an argument register by number *)
let codegenx86_argreg = function
  | 0 -> "%rdi"
  | 1 -> "%rsi"
  | 2 -> "%rdx"
  | 3 -> "%rcx"
  | 4 -> "%r8"
  | 5 -> "%r9"
  | n -> ((n - 4) * 8 |> string_of_int) ^ "(%rbp)"

(* Instruction to push an argument *)
let codegenx86_pusharg n =
  "\tpushq\t" ^ codegenx86_argreg n ^ "\n"
  |> add_string code

(* Instruction to pop an argument *)
let codegenx86_poparg n =
  "\tpopq\t" ^ codegenx86_argreg n ^ "\n"
  |> add_string code

(* Lookup the address for a value *)
let rec lookup x = function
  | [] -> failwith "Could not find symbol address."
  | (y, addr)::ys when x = y -> addr
  | _::ys -> lookup x ys

(* Generate x86 code for expressions *)
let rec codegenx86 symt = function
  | Empty ->
    codegenx86_empty ();
    sp := !sp + 1
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
    sp := !sp - 1
  | New (x, e1, e2) ->
    codegenx86 symt e1;
    codegenx86_ptr ();
    sp := !sp + 1;
    codegenx86 ((x, !sp) :: symt) e2;
    codegenx86_new ();
    sp := !sp - 1
  | Seq (e, Empty) -> codegenx86 symt e
  | Seq (e1, e2) ->
    codegenx86 symt e1;
    codegenx86_pop ();
    sp := !sp - 1;
    codegenx86 symt e2
  | Asg (e1, e2) ->
    codegenx86 symt e1;
    codegenx86 symt e2;
    codegenx86_asg ();
    sp := !sp - 1
  | Deref n ->
    codegenx86 symt n;
    codegenx86_deref ()
  | Print n ->
    codegenx86 symt n;
    codegenx86_print ();
    codegenx86_empty ()
  | If (x, e1, e2) ->
    codegenx86 symt x;
    codegenx86_testjz ();
    lblp := !lblp + 1;
    sp := !sp - 1;
    codegenx86 symt e1;
    codegenx86_jmplbl !lblp (!lblp - 1);
    lblp := !lblp + 1;
    codegenx86 symt e2;
    codegenx86_lbl (!lblp - 1)
  | While (x, e) ->
    lblp := !lblp + 1;
    codegenx86_jmplbl (!lblp - 1) !lblp;
    codegenx86 symt e;
    codegenx86_pop ();
    sp := !sp - 1;
    codegenx86_lbl (!lblp - 1);
    codegenx86 symt x;
    codegenx86_testjnz ();
    codegenx86_empty ();
    lblp := !lblp + 1
  | Application (Identifier n, e) ->
    let args = make_list e in
    List.iteri (fun i x ->
        codegenx86 symt x;
        codegenx86_poparg i;
        sp := !sp - 1
      ) args;
    codegenx86 symt e;
    codegenx86_call n;
    sp := !sp + 1
  | _ -> failwith "Unimplemented expression."

(* Push arguments onto the stack and generate symt *)
let rec codegenx86_addargs count = function
  | []      -> []
  | x::xs   ->
    codegenx86_pusharg count;
    sp := !sp + 1;
    let addr = !sp in
    (x, addr) :: (codegenx86_addargs (count + 1) xs)

(* Generate x86 code for a function *)
let codegenx86_func name args exp =
  let oldsp = !sp in
  sp := 0;
  codegenx86_newfunc name;
  let symt = codegenx86_addargs 0 (List.rev args) in
  codegenx86 symt exp;
  codegenx86_endfunc ();
  sp := !sp - 1;
  sp := oldsp

(* Generate x86 code for the main function *)
let codegenx86_main exp =
  codegenx86_newfunc "main";
  codegenx86 [] exp

(* Generate x86 injection code for a program *)
let rec codegenx86_prog = function
  | []                              -> failwith "Codegenx86 requires a 'main' function."
  | Fundef ("main", args, body)::ys -> codegenx86_main body
  | Fundef (name, args, body)::ys   ->
    codegenx86_func name args body;
    codegenx86_prog ys

(* Generate x86 code wrapped inside an x86 template *)
let compile prog =
  reset code;
  add_string code codegenx86_prefix;
  codegenx86_prog prog;
  add_string code codegenx86_suffix;
  output_buffer stdout code;
  ""
