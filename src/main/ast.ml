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

let opcode_string = function
  | Plus -> " Plus "
  | Minus -> " Minus "
  | Times -> " Times "
  | Divide -> " Divide "
  | Leq -> " Leq "
  | Geq -> " Geq "
  | Equal -> " Equal "
  | Noteq -> " Noteq "
  | And -> " And "
  | Or -> " Or "
  | Not -> " Not "


let rec exp_string = function
  | Empty -> "empty"
  | Seq (e, f) -> "Seq ( " ^ exp_string e ^ "; " ^ exp_string f ^ " ) "
  | While (e, f) -> "While ( " ^ exp_string e ^ " ) Do { " ^ exp_string f ^ " } "
  | If (e, f, g) -> "If ( " ^ exp_string e ^ " ) Do { " ^ exp_string f ^ " } Else { " ^ exp_string g ^ " } "
  | Asg (e, f) -> "Asg ( " ^ exp_string e ^ " := " ^ exp_string f ^ " ) "
  | Deref e -> "Deref (" ^ exp_string e ^ ")"
  | Operator (op, e, f) -> "Operator ( " ^ opcode_string op ^ exp_string e ^ " " ^ exp_string f ^ " ) "
  | Application (e, f) -> "Application ( " ^ exp_string e ^ " ( " ^ exp_string f ^ " ) "
  | Const i -> "Const " ^ string_of_int i
  | Readint -> "Readint () "
  | Printint e -> "Printint ( " ^ exp_string e ^ " ) "
  | Identifier s -> "\"" ^ s ^ "\""
  | Let (s, e, f) -> "Let ( \"" ^ s ^ "\" = " ^ exp_string e ^ " ) In { " ^ exp_string f ^ " } "
  | New (s, e, f) -> "New ( \"" ^ s ^ "\" = " ^ exp_string e ^ " ) In { " ^ exp_string f ^ " } "

let function_string = function
  | Function (name, args, body) -> "Function " ^ name ^ " ( " ^ String.concat ", " args  ^ " ) { " ^ exp_string body ^ " }"
