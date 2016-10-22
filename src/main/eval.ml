open Ast

(* Hash table for storing values *)
let store = Hashtbl.create 100

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
  | Not     ->  not f
  | _       -> failwith "Operator must be of type bool."

(* Choose correct operator evaluation depending on types *)
let eval_operator op e f = match e, f with
  | Int e, Int f -> (match op with
      | Plus | Minus | Times | Divide   -> Int (eval_operator_int op e f)
      | Leq  | Geq   | Equal | Noteq    -> Bool (eval_operator_compare op e f)
      | _                               -> failwith "Operator cannot be applied to integers.")

  | Bool e, Bool f -> (match op with
      | And | Or    -> Bool (eval_operator_bool op e f)
      | _           -> failwith "Operator cannot be applied to booleans.")

  | e, Bool f -> (match op with
      | Not -> Bool (not f)
      | _   -> failwith "Operator must be of type not.")

  | _ -> failwith "Values are of different types. Operator cannot be applied."

(* Evaluate an expression *)
let rec eval_exp = function
  | Empty           -> Unit
  | Const e         -> Int e
  | Identifier e    -> Var e

  | Deref e -> (match eval_exp e with
      | Var x   ->  Hashtbl.find store x
      | _       -> failwith "Can only dereference a variable.")

  | While (e, f) -> (match eval_exp e with
      | Bool true   -> ignore (eval_exp f); eval_exp (While (e, f))
      | Bool false  -> Unit
      | _           -> failwith "Invalid value for while condition.")

  | If (e, f, g) -> (match eval_exp e with
      | Bool true   -> eval_exp f
      | Bool false  -> eval_exp g
      | _           -> failwith "Invalid value for if condition.")

  | Operator (op, e, f)     -> eval_operator op (eval_exp e) (eval_exp f)
  | Asg (Identifier x, e2)  -> let v2 = eval_exp e2 in Hashtbl.replace store x v2; v2
  | Seq (e, Empty)          -> eval_exp e
  | Seq (e, f)              -> let _ = eval_exp e in let v2 = eval_exp f in v2

  | Printint e -> (match eval_exp e with
      | Int e   -> print_endline (string_of_int e); Unit
      | _       -> failwith "Printint can only print integers.")

  | Application _   -> failwith "Application evaluation not supported."
  | Readint         -> failwith "Readint evaluation not supported."
  | Let _           -> failwith "Let evaluation not supported."
  | New _           -> failwith "New evaluation not supported."
  | _               -> failwith "Cannot evaluate unsupported expression."

(* Evaluate a function *)
let eval_function = function
  | (name, args, body) -> eval_exp body
