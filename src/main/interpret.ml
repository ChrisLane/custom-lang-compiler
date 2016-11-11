open Ast
open Hashtbl

(* Configuration *)
let ram = create 100
let acc = ref 0

(* Instruction execution *)
let op (op, addr1, addr2) = acc := op (find ram addr1) (find ram addr2)
let st addr = replace ram addr !acc
let ldc n = acc := n
let mv addr2 addr1 = replace ram addr1 (find ram addr2)
(* jz code is below interpret *)

(* Address initialising *)
let addr_base = ref 0
let new_addr() = addr_base := !addr_base + 1; !addr_base

(* Find an address *)
let rec lookup x = function
  | []                          -> failwith "Could not find a variable x."
  | (y, addr)::_   when x = y   -> addr
  | _::ys                       -> lookup x ys

(* Return the function of an operator *)
let fun_of_op = function
  | Plus    -> ( + )
  | Minus   -> ( - )
  | Times   -> ( * )
  | Divide  -> ( / )
  | Leq     -> (fun x y -> if x <= y then 1 else 0)
  | Geq     -> (fun x y -> if x >= y then 1 else 0)
  | Equal   -> (fun x y -> if x = y then 1 else 0)
  | Noteq   -> (fun x y -> if x != y then 1 else 0)
  | And     -> (fun x y -> if x=1 && y=1 then 1 else 0)
  | Or      -> (fun x y -> if x=1 || y=1 then 1 else 0)
  | Not     -> (fun x y -> if y=1 then 0 else 1)

(* Interpret an expression *)
let rec interpret symt = function
  | Empty -> !addr_base
  | Operator (oper, e1, e2) ->
    let addr1 = interpret symt e1 in
    let addr2 = interpret symt e2 in
    op (fun_of_op oper, addr1, addr2);
    addr_base := addr1;
    st addr1;
    addr1
  | Identifier x ->
    let addr = lookup x symt in
    let addr' = new_addr() in
    mv addr addr';
    addr'
  | Const n ->
    let addr = new_addr() in
    ldc n;
    st addr;
    addr
  | Bool n ->
    let b = if n then 1 else 0 in
    interpret symt (Const b)
  | Seq (e, Empty) -> interpret symt e
  | Seq (e, f) ->
    let _ = interpret symt e in
    interpret symt f
  | If (n, e, f) ->
    let addr1 = interpret symt n in
    let addr2 = jnz symt addr1 e f in
    mv addr2 addr1;
    addr_base := addr1;
    st addr1;
    addr1
  | While (n, e) ->
    let addr1 = interpret symt n in
    ignore (jnz symt addr1 (Seq (e, While (n, e))) Empty);
    addr_base := addr1;
    addr1;
  | Let (x, e1, e2) ->
    let addr1 = interpret symt e1 in
    let addr2 = interpret ((x, addr1) :: symt) e2 in
    mv addr2 addr1;
    addr_base := addr1;
    addr1
  | _ -> failwith "Not implemented. interpret."

and jnz symt n e f =
  if (find ram n) != 0 then interpret symt e else interpret symt f

(* Interpret a function *)
let interpret_func func =
  let addr = interpret [] func in
  find ram addr
  |> string_of_int

(* Interpret a program *)
let rec interpret_prog = function
  | []                              -> failwith "Interpret requires a 'main' function."
  | Fundef ("main", args, body)::ys   -> interpret_func body
  | _::ys                           -> interpret_prog ys
