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

type fundef = string * string list * expression

type program = fundef list

let rec make_seq = function
  | [] -> Empty
  | [x] -> x
  | x :: xs -> Seq (x, make_seq xs)

let opcode_string = function
  | Plus -> "Plus"
  | Minus -> "Minus"
  | Times -> "Times"
  | Divide -> "Divide"
  | Leq -> "Leq"
  | Geq -> "Geq"
  | Equal -> "Equal"
  | Noteq -> "Noteq"
  | And -> "And"
  | Or -> "Or"
  | Not -> "Not"

let rec indentrec i s = match i with
  | 0 -> s ^ ""
  | i -> indentrec (i-1) (s ^ "    ");;

let indent i = indentrec i "";;

let rec exp_string e i = match e with
  | Empty -> (indent i) ^ "empty "
  | Seq (e, f) -> exp_string e i ^ "; \n" ^ exp_string f i
  | While (e, f) -> (indent i) ^ "While ( " ^ exp_string e 0 ^ ") { \n" ^ exp_string f (i+1) ^ "\n" ^ indent i ^ "} "
  | If (e, f, g) -> (indent i) ^ "If ( " ^ exp_string e 0 ^ ") { \n" ^ exp_string f (i+1)  ^ "\n" ^ indent i ^ "} Else { \n" ^ exp_string g (i+1)  ^ "\n" ^ indent i ^ "} "
  | Asg (e, f) -> (indent i) ^ "Asg ( " ^ exp_string e 0 ^ ":= " ^ exp_string f 0 ^ ") "
  | Deref e -> (indent i) ^ "Deref ( " ^ exp_string e 0 ^ ") "
  | Operator (Not, Empty, e) -> (indent i) ^ "Operator ( Not, " ^ exp_string e 0 ^ ") "
  | Operator (op, e, f) -> (indent i) ^ "Operator ( " ^ opcode_string op ^ ", " ^ exp_string e 0 ^ ", " ^ exp_string f 0 ^ ") "
  | Application (e, f) -> (indent i) ^ "Application ( " ^ exp_string e 0 ^ "( " ^ exp_string f 0 ^ ") "
  | Const n -> (indent i) ^ "Const " ^ string_of_int n ^ " "
  | Readint -> (indent i) ^ "Readint () "
  | Printint e -> (indent i) ^ "Printint ( " ^ exp_string e 0 ^ ") "
  | Identifier s -> (indent i) ^ "\"" ^ s ^ "\" "
  | Let (s, e, f) -> (indent i) ^ "Let ( \"" ^ s ^ "\" = " ^ exp_string e 0 ^ ") In { \n" ^ exp_string f (i+1) ^ "\n" ^ indent i ^ "} "
  | New (s, e, f) -> (indent i) ^ "New ( \"" ^ s ^ "\" = " ^ exp_string e 0 ^ ") In { \n" ^ exp_string f (i+1) ^ "\n" ^ indent i ^ "} "

let function_string = function
  | (name, args, body) -> name ^ " ( " ^ String.concat ", " args  ^ " ) { \n" ^ exp_string body 1 ^ "\n}"
