open Ast
open Buffer
open Template

let code = create 50
let sp = ref 0
let lblp = ref 0
let exitp = ref 0
let testp = ref 0

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
  | Lt      -> "\tcmpq\t%rbx, %rax\n" ^ "\tsetl\t%al\n" ^ "\tmovzbq\t%al, %rax\n"
  | Gt      -> "\tcmpq\t%rbx, %rax\n" ^ "\tsetg\t%al\n" ^ "\tmovzbq\t%al, %rax\n"
  | Equal   -> "\tcmpq\t%rbx, %rax\n" ^ "\tsete\t%al\n" ^ "\tmovzbq\t%al, %rax\n"
  | Noteq   -> "\tcmpq\t%rbx, %rax\n" ^ "\tsetne\t%al\n" ^ "\tmovzbq\t%al, %rax\n"
  | And     -> "\tandq\t%rbx, %rax\n"
  | Or      -> "\torq\t%rbx, %rax\n"
  | Not     -> "\tcmpq\t%rbx, %rax\n" ^ "\tsetz\t%al\n" ^ "\tmovzbq\t%al, %rax\n"

(* Instructions for empty *)
let asm_empty _ =
  "\tpushq\t$0\n"
  |> add_string code

(* Instructions for a const *)
let asm_const n =
  "\tpushq\t$" ^ (string_of_int n) ^ "\n"
  |> add_string code

(* Instructions for a bool*)
let asm_bool n =
  "\tpushq\t$" ^ (string_int_of_bool n) ^ "\n"
  |> add_string code

(* Instructions for an operator *)
let asm_op op =
  "\tpopq\t%rbx\n" ^
  "\tpopq\t%rax\n" ^
  (instruction_of_op op) ^
  "\tpushq\t%rax\n"
  |> add_string code

(* Instructions for an identifier *)
let asm_id addr =
  "\t//offset\t" ^ (string_of_int addr) ^ "\n" ^
  "\tmovq\t" ^ (-16 -8 * addr |> string_of_int) ^ "(%rbp), %rax\n" ^
  "\tpushq\t%rax\n"
  |> add_string code

(* Instructions for a let *)
let asm_let _ =
  "\tpopq\t%rax\n" ^
  "\tpopq\t%rbx\n" ^
  "\tpushq\t%rax\n"
  |> add_string code

(* Instructions for a new variable *)
let asm_new _ =
  "\tpopq\t%rax\n" ^
  "\tpopq\t%rbx\n" ^
  "\tpushq\t%rax\n"
  |> add_string code

(* Instructions for creating a pointer *)
let asm_ptr _ =
  "\tleaq\t" ^ (-16 -8 * !sp |> string_of_int) ^ "(%rbp), %rax\n" ^
  "\tpushq\t%rax\n"
  |> add_string code

(* Instructions for dereferencing *)
let asm_deref _ =
  "\tpopq\t%rbx\n" ^
  "\tmovq\t(%rbx), %rax\n" ^
  "\tpushq\t%rax\n"
  |> add_string code

(* Instructions for assignment *)
let asm_asg _ =
  "\tpopq\t%rax\n" ^
  "\tpopq\t%rbx\n" ^
  "\tmovq\t%rax, (%rbx)\n" ^
  "\tpushq\t%rax\n"
  |> add_string code

(* Instructions for dereferencing *)
let asm_deref _ =
  "\tpopq\t%rbx\n" ^
  "\tmovq\t(%rbx), %rax\n" ^
  "\tpushq\t%rax\n"
  |> add_string code

(* Instruction to pop from the stack *)
let asm_pop _ =
  "\tpopq\t%rax\n"
  |> add_string code

(* Instructions for print *)
let codegenx86_print _ =
  "\tpopq\t%rdi\n" ^
  "\tcall\tprint\n"
  |> add_string code

(* Generate a label *)
let asm_lbl n =
  ".L" ^ (string_of_int n) ^ ":\n"
  |> add_string code

(* Instruction to break out of a loop *)
let asm_break _ =
  "\tjmp .L" ^ (string_of_int (!exitp)) ^ "\n"
  |> add_string code

(* Instruction to jump *)
let asm_jmp x =
  "\tjmp .L" ^ (string_of_int x) ^ "\n"
  |> add_string code

(* Instruction to continue to a loop's test label *)
let asm_continue _ =
  "\tjmp .L" ^ (string_of_int (!testp)) ^ "\n"
  |> add_string code

(* Instructions to test and jump if gate zero *)
let asm_testjz x =
  "\tpopq\t%rax\n" ^
  "\ttest\t%rax, %rax\n" ^
  "\tjz .L" ^ (string_of_int x) ^ "\n"
  |> add_string code

(* Instructions to test and jump if gate not zero *)
let asm_testjnz x =
  "\tpopq\t%rax\n" ^
  "\ttest\t%rax, %rax\n" ^
  "\tjnz .L" ^ (string_of_int x) ^ "\n"
  |> add_string code

(* Instructions that come before a function definition *)
let asm_newfunc n =
  "\t.globl\t" ^ n ^ "\n" ^
  "\t.type\t" ^ n ^ ", @function\n" ^
  n ^ ":\n" ^
  "\tpushq\t%rbp\n" ^
  "\tmovq\t%rsp, %rbp\n" ^
  "\tsubq\t$16, %rsp\n"
  |> add_string code

(* Instructions for the end of a function *)
let asm_endfunc _ =
  "\tpopq\t%rax\n" ^
  "\tleave\n" ^
  "\tret\n"
  |> add_string code

(* Instructions to call a function and store it's value *)
let asm_call n =
  "\tcall\t" ^ n ^ "\n" ^
  "\tpushq\t%rax\n"
  |> add_string code

(* A function to get an argument register by number *)
let asm_argreg = function
  | 0 -> "%rdi"
  | 1 -> "%rsi"
  | 2 -> "%rdx"
  | 3 -> "%rcx"
  | 4 -> "%r8"
  | 5 -> "%r9"
  | n -> ((n - 4) * 8 |> string_of_int) ^ "(%rbp)"

(* Instruction to push an argument *)
let asm_pusharg n =
  "\tpushq\t" ^ asm_argreg n ^ "\n"
  |> add_string code

(* Instruction to pop an argument *)
let asm_poparg n =
  "\tpopq\t" ^ asm_argreg n ^ "\n"
  |> add_string code

(* Lookup the address for a value *)
let rec lookup x = function
  | [] -> failwith "Could not find symbol address."
  | (y, addr)::ys when x = y -> addr
  | _::ys -> lookup x ys

(* Generate x86 code for expressions *)
let rec codegenx86 symt = function
  | Empty ->
    add_string code "// begin empty\n";
    asm_empty ();
    add_string code "// end empty\n";
    sp := !sp + 1

  | Operator (op, e1, e2) ->
    add_string code "// begin operator\n";
    codegenx86 symt e1;
    codegenx86 symt e2;
    asm_op op;
    add_string code "// end operator\n";
    sp := !sp - 1

  | Identifier x ->
    add_string code "// begin identifier\n";
    let addr = lookup x symt in
    asm_id (addr);
    add_string code "// end identifier\n";
    sp := !sp + 1

  | Const n ->
    asm_const n;
    sp := !sp + 1

  | Bool n ->
    asm_bool n;
    sp := !sp + 1

  | Let (x, e1, e2) ->
    add_string code "// begin let\n";
    codegenx86 symt e1;
    codegenx86 ((x, !sp) :: symt) e2;
    asm_let ();
    add_string code "// end let\n";
    sp := !sp - 1

  | New (x, e1, e2) ->
    add_string code "// begin new\n";
    codegenx86 symt e1;
    asm_ptr ();
    sp := !sp + 1;
    codegenx86 ((x, !sp) :: symt) e2;
    asm_new ();
    add_string code "// end new\n";
    sp := !sp - 2

  | Seq (e, Empty) ->
    add_string code "// begin single seq\n";
    codegenx86 symt e;
    add_string code "// end single seq\n"
  | Seq (e1, e2) ->
    add_string code "// begin seq\n";
    codegenx86 symt e1;
    asm_pop ();
    sp := !sp - 1;
    codegenx86 symt e2;
    add_string code "// end seq\n"

  | Asg (e1, e2) ->
    add_string code "// begin asg\n";
    codegenx86 symt e1;
    codegenx86 symt e2;
    asm_asg ();
    asm_empty ();
    sp := !sp + 1;
    add_string code "// end asg\n"

  | Deref n ->
    add_string code "// begin deref\n";
    codegenx86 symt n;
    asm_deref ();
    add_string code "// end deref\n"

  | Print n ->
    add_string code "// begin print\n";
    codegenx86 symt n;
    codegenx86_print ();
    asm_empty ();
    add_string code "// end print\n"

  | Return n ->
    add_string code "// begin return\n";
    let _ = codegenx86 symt n in
    asm_endfunc ();
    add_string code "// end return\n";
    sp := !sp - 1

  | Break ->
    add_string code "// begin break\n";
    asm_break ();
    add_string code "// end break\n"

  | Continue ->
    add_string code "// begin continue\n";
    asm_continue ();
    add_string code "// end continue \n"

  | If (x, e1, e2) ->
    add_string code "// begin if\n";
    let falselbl = !lblp in
    lblp := !lblp + 1;
    let endlbl = !lblp in
    lblp := !lblp + 1;
    let oldexit = !exitp in
    let oldtest = !testp in
    codegenx86 symt x;
    (* Jump to label 0 *)
    asm_testjz falselbl;
    sp := !sp - 1;
    add_string code "// begin if statement true\n";
    codegenx86 symt e1;
    asm_jmp endlbl;
    add_string code "// end if statement true\n";
    (* Label 0 *)
    asm_lbl falselbl;
    lblp := !lblp + 1;
    add_string code "// begin if statement false\n";
    codegenx86 symt e2;
    add_string code "// end if statement false\n";
    exitp := oldexit;
    testp := oldtest;
    (* Label 1 *)
    asm_lbl endlbl;
    add_string code "// end if\n"

  | While (x, e) ->
    add_string code "// begin while\n";
    let oldsp = !sp in
    let test = !lblp in
    lblp := !lblp + 1;
    let body = !lblp in
    lblp := !lblp + 1;
    let finish = !lblp in
    lblp := !lblp + 1;
    (* Label 1 *)
    asm_lbl body;
    let oldexit = !exitp in
    let oldtest = !testp in
    exitp := finish;
    testp := test;
    add_string code "// begin while body\n";
    codegenx86 symt e;
    add_string code "// end while body\n";
    asm_pop ();
    sp := oldsp;
    exitp := oldexit;
    testp := oldtest;
    (* Label 0 *)
    asm_lbl test;
    codegenx86 symt x;
    (* Jump to label 1 *)
    asm_testjnz body;
    sp := !sp - 1;
    (* Label 2 *)
    asm_lbl finish;
    add_string code "// end while\n"

  | Application (Identifier n, e) ->
    add_string code "// begin application\n";
    let args = make_list e in
    List.iteri (fun i x ->
        codegenx86 symt x;
        asm_poparg i;
        sp := !sp - 1
      ) args;
    codegenx86 symt e;
    asm_call n;
    add_string code "// end application\n";
    sp := !sp + 1

  | _ -> failwith "Unimplemented expression for codegen."

(* Push arguments onto the stack and generate symt *)
let rec codegenx86_addargs count = function
  | []      ->
    add_string code "// end function definition arguments\n";
    []
  | x::xs   ->
    add_string code "// begin function definition arguments\n";
    asm_pusharg count;
    sp := !sp + 1;
    let addr = !sp in
    (x, addr) :: (codegenx86_addargs (count + 1) xs)

(* Generate x86 code for a function *)
let codegenx86_func name args exp =
  let oldsp = !sp in
  sp := 0;
  add_string code "// begin function definition\n";
  asm_newfunc name;
  let symt = codegenx86_addargs 0 args in
  codegenx86 symt exp;
  asm_endfunc ();
  add_string code "// end function definition\n";
  sp := oldsp

(* Generate x86 code for the main function *)
let codegenx86_main exp =
  asm_newfunc "main";
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
