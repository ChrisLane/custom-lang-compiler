open Buffer
open Ast
open Interpret

(* Generate an assembly string for an operator *)
let string_of_operator = function
  | Plus    -> "add"
  | Minus   -> "sub"
  | Times   -> "mul"
  | Divide  -> "div"
  | _       -> failwith "Unimplemented operator to convert to string."

(* Hashtable for storing code *)
let code = create 100

(* Generate strings of code for each assembly instruction *)
(* Operator *)
let codegen_op (op, addr1, addr2) =
  (string_of_operator op) ^ " r" ^ (string_of_int addr1) ^ ", r" ^ (string_of_int addr2)
  ^ "\n" |> add_string code
(* Store *)
let codegen_st addr = "st r" ^ (string_of_int addr)
                      ^ "\n" |> add_string code
(* Load *)
let codegen_ldc n = "ld " ^ (string_of_int n)
                    ^ "\n" |> add_string code
(* Move *)
let codegen_mv (addr2, addr1) =
  "mv r" ^ (string_of_int addr2) ^ ", r" ^ (string_of_int addr1)
  ^ "\n" |> add_string code


(* Generate code for an expression *)
let rec codegen symt = function
  | Operator (op, e1, e2) ->
    let addr1 = codegen symt e1 in
    let addr2 = codegen symt e2 in
    codegen_op (op, addr1, addr2);
    addr_base := addr1;
    codegen_st addr1;
    addr1
  | Identifier x ->
    let addr = lookup x symt in
    let addr' = new_addr() in
    codegen_mv (addr, addr');
    addr'
  | Const n ->
    let addr = new_addr() in
    codegen_ldc n;
    codegen_st addr;
    addr
  | Bool n ->
    let b = if n then 1 else 0 in
    codegen symt (Const b)
  | Seq (e, Empty) -> codegen symt e
  | Seq (e, f) ->
    let _ = codegen symt e in
    codegen symt f
  | Let (x, e1, e2) ->
    let addr1 = codegen symt e1 in
    let addr2 = codegen ((x, addr1) :: symt) e2 in
    codegen_mv (addr2, addr1);
    addr_base := addr1;
    addr1
  | _ -> failwith "Not implemented. codegen."

(* Generate code for a function *)
let codegen_func func =
  reset code;
  addr_base := 0;
  let addr = codegen [] func in
  output_buffer stdout code;
  "ld r" ^ (string_of_int addr)

(* Generate code for a program *)
let rec codegen_prog = function
  | []                              -> failwith "Codegen requires a 'main' function."
  | Fundef ("main", args, body)::ys -> codegen_func body
  | _::ys                           -> codegen_prog ys
