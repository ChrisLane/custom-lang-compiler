open Ast

(* Hash table for storing global variable values *)
let store = Hashtbl.create 100

(* Hash table for storing function definitions *)
let functions = Hashtbl.create 100

(* Evaluate operators for integer manipulation (Return Int) *)
let eval_operator_int op e f = match op with
  | Plus    -> e + f
  | Minus   -> e - f
  | Times   -> e * f
  | Divide  -> e / f
  | _       -> failwith "Operator must be of type int."

(* Evaluate operators for comparison (Return Bool) *)
let eval_operator_compare op e f = match op with
  | Leq     -> e <= f
  | Geq     -> e >= f
  | Equal   -> e =  f
  | Noteq   -> e != f
  | _       -> failwith "Operator must be of comparison type."

(* Evaluate operators for booleans (Return Bool) *)
let eval_operator_bool op e f = match op with
  | And     -> e && f
  | Or      -> e || f
  | _       -> failwith "Operator must be of type bool."

(* Choose correct operator evaluation depending on types *)
let eval_operator op e f = match e, f with
  | DInt e, DInt f -> (match op with
      | Plus | Minus | Times | Divide   -> DInt (eval_operator_int op e f)
      | Leq  | Geq   | Equal | Noteq    -> DBool (eval_operator_compare op e f)
      | _                               -> failwith "Operator cannot be applied to integers.")

  | DBool e, DBool f -> (match op with
      | And | Or    -> DBool (eval_operator_bool op e f)
      | _           -> failwith "Operator cannot be applied to booleans.")

  | e, DBool f -> (match op with
      | Not -> DBool (not f)
      | _   -> failwith "Operator must be of type not.")

  | _ -> failwith "Values are of different types. Operator cannot be applied."

(* Find a variable in variable storage and return it's value *)
let rec lookup x = function
  | []                          -> failwith "Could not find a variable and value during lookup."
  | (y, z)::ys      when x = y  -> z
  | y::ys                       -> lookup x ys

(* Find and return an entry in the environment *)
let rec lookupenv x = function
  | []                          -> failwith "Could not find a variable during environment lookup"
  | (y, z)::ys      when x = y  -> (y, z)
  | y::ys                       -> lookupenv x ys

(* Find a variable in variable storage and update it's value *)
let rec update x v = function
  | []                          -> failwith "Could not find a variable to update."
  | (y, DRef z)::ys  when x = y  -> Hashtbl.replace store (string_of_int z) v
  | y::ys                       -> update x v ys

(* References for variable pointers *)
let addr_gbl = ref 0
let newref() = addr_gbl:=!addr_gbl+1; !addr_gbl

(* Evaluate an expression *)
let rec eval_exp e env = match e with
  | Empty           -> DUnit
  | Const e         -> DInt e
  | Bool e          -> DBool e
  | Identifier e    -> lookup e env

  | Deref e -> (match eval_exp e env with
      | DRef z  -> Hashtbl.find store (string_of_int z)
      | _       -> failwith "Can only dereference a variable.")

  | While (e, f) -> (match eval_exp e env with
      | DBool true   -> ignore (eval_exp f env); eval_exp (While (e, f)) env
      | DBool false  -> DUnit
      | _           -> failwith "Invalid value for while condition.")

  | If (e, f, g) -> (match eval_exp e env with
      | DBool true   -> eval_exp f env
      | DBool false  -> eval_exp g env
      | _           -> failwith "Invalid value for if condition.")

  | Operator (op, e, f)     -> eval_operator op (eval_exp e env) (eval_exp f env)
  | Asg (Identifier x, e)   -> let v = eval_exp e env in update x v env; DUnit
  | Seq (e, Empty)          -> eval_exp e env
  | Seq (e, f)              -> ignore (eval_exp e env);  eval_exp f env

  | Print e -> (match eval_exp e env with
      | DInt e   -> print_endline (string_of_int e); DUnit
      | DBool e  -> print_endline (string_of_bool e); DUnit
      | DVar e   -> print_endline e; DUnit
      | _       -> failwith "Printint can only print integers.")

  | Application (Identifier n, e) ->
    if Hashtbl.mem functions n
    then
      let args = list_args e in
      (match Hashtbl.find functions n with
        | Fundef (name, _, body) -> eval_function env (Fundef (name, args, body)))
    else
      failwith "No such function."

  | Readint                 -> DInt (read_int())
  | Let (x, e1, e2)         -> let v1 = eval_exp e1 env in eval_exp e2 ((x, v1)::env)
  | New (x, e1, e2)         ->
    let v1 = eval_exp e1 env in
    let l = newref() in
    let v2 = Hashtbl.add store (string_of_int l) v1;    eval_exp e2 ((x, DRef l)::env) in
    Hashtbl.remove store (string_of_int l);             v2

  | _                       -> failwith "Cannot evaluate unsupported expression."

(* Produce a string list of application arguements for function evaluation *)
and addargs env = function
  | []      -> []
  | x::xs   -> (lookupenv x env)::(addargs env xs)

(* Evaluate a function *)
and eval_function env = function
  | Fundef (name, args, body) -> eval_exp body (addargs env args)

(* Evaluate a program *)
let eval_program = function
  | Fundef ("main", args, body) -> eval_exp body []
  | Fundef (name, args, body)   -> Hashtbl.add functions name (Fundef (name, args, body)); DUnit
