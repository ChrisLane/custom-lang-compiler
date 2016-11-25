(* Data types *)
type dtype =
  | DInt of int
  | DRef of int
  | DBool of bool
  | DVar of string
  | DUnit

(* Operators *)
type opcode =
  | Plus | Minus | Times | Divide
  | Leq  | Geq   | Equal | Noteq
  | And  | Or    | Not

(* Expressions *)
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
  | Bool of             bool                                    (* true *)
  | Ref of              int                                     (* 1 *)
  | Unknown                                                     (* readint() *)
  | Readint                                                     (* read_int () *)
  | Print of            expression                              (* print_int (e) *)
  | Identifier of       string                                  (* x *)
  | Let of              string * expression * expression        (* let x = e in e *)
  | New of              string * expression * expression        (* new x = e in e *)
  | Return of           expression                              (* return e *)

(* Function *)
type fundef = Fundef of string * string list * expression

(* Program *)
type program = fundef list

(* Convert a list to a Seq expression*)
let rec make_seq = function
  | [] -> Empty
  | [x] -> x
  | x :: xs -> Seq (x, make_seq xs)

let rec make_list = function
  | Empty       -> []
  | Seq (e, f)  -> e::(make_list f)
  | e           -> [e]

(* Convert a Seq expression to a list of strings*)
let rec list_args = function
  | Empty                   -> []
  | Identifier e            -> [e]
  | Seq (Identifier e, f)   -> e::(list_args f)
  | _                       -> failwith "Bad input for args as list."
