type opcode =
  | Plus | Minus | Times | Divide
  | Leq | Geq | Equal | Noteq
  | And | Or | Not

type expression =
  | Empty
  | Seq of              expression * expression                 (* e; e *)
  | While of            expression * expression                 (* while e do e *)
  | If of               expression * expression * expression    (* if e do e else e *)
  | Asg of              expression * expression                 (* e := e *)
  | Deref of            expression                              (* !e *)
  | Operator of         opcode * expression * expression        (* e + e *)
  | Application of      expression * expression                 (* e(e) *)
  | Const of            int                                     (* 7 *)
  | Readint                                                     (* read_int () *)
  | Printint of         expression                              (* print_int (e) *)
  | Identifier of       string                                  (* x *)
  | Let of              string * expression * expression        (* let x = e in e *)
  | New of              string * expression * expression        (* new x = e in e *)

type fundef = Function of  string * string list * expression

type program = fundef list

let rec make_seq = function
  | [] -> Empty
  | x :: xs -> Seq (x, make_seq xs)
