open Ast
open Hashtbl

let store = create 100

let optim_operator_const e f = function
  | Plus    -> e + f
  | Minus   -> e - f
  | Times   -> e * f
  | Divide  -> e / f
  | _       -> failwith "Operator must be of type int."

let optim_operator_compare e f = function
  | Leq     -> e <= f
  | Geq     -> e >= f
  | Equal   -> e =  f
  | Noteq   -> e != f
  | _       -> failwith "Operator must be of comparison type."

let optim_operator_bool e f = function
  | And -> e && f
  | Or  -> e || f
  | _   -> failwith "Operator must be of type bool."

let optim_operator op e f = match e, f with
  | Const e, Const f -> (match op with
      | Plus | Minus | Times | Divide   -> Const (optim_operator_const e f op)
      | Leq  | Geq   | Equal | Noteq    -> Bool (optim_operator_compare e f op)
      | _                               -> failwith "Operation cannot be applied to consts.")
  | Bool e, Bool f -> (match op with
      | And | Or    -> Bool (optim_operator_bool e f op)
      | _           -> failwith "Operator cannot be applied to booleans.")
  | e, Bool f -> (match op with
      | Not -> Bool (not f)
      | _   -> failwith "Operator must be of type not.")
  | _ -> Operator (op, e, f)

(* Find a variable in variable storage and return it's value *)
let rec lookup x = function
  | []                          -> Unit
  | (y, Ref z)::_  when x = y   -> find store (string_of_int z)
  | (y, z)::_      when x = y   -> z
  | _::ys                       -> lookup x ys

(* References for variable pointers *)
let addr_gbl = ref 0
let newref() = addr_gbl:=!addr_gbl+1; !addr_gbl

let rec optim_exp env = function
  | Empty                   -> Empty
  | Const e                 -> Const e

  | Identifier e            -> (match lookup e env with
    | Unit      -> Identifier e
    | Int f     -> Const f
    | Bool f    -> Bool f
    | Var f     -> Identifier f
    | _         -> failwith "balls")

  | Deref e                 -> Deref (optim_exp env e)

  | While (e, f)            -> (match optim_exp env e with
      | Bool false  -> Empty
      | Bool true   -> optim_exp env (Seq (f, e))
      | n           -> While (n, optim_exp env f))

  | If (e, f, g)            -> (match optim_exp env e with
      | Bool true     -> optim_exp env f
      | Bool false    -> optim_exp env g
      | n             -> If (n, optim_exp env f, optim_exp env g))

  | Operator (op, e, f)     -> optim_operator op (optim_exp env e) (optim_exp env f)
  | Asg (e, f)              -> let v = optim_exp env f in (*update e v env;*) Asg (e, v)
  | Seq (e, Empty)          -> optim_exp env e
  | Seq (e, f)              -> Seq (optim_exp env e, optim_exp env f)
  | Print e                 -> Print (optim_exp env e)
  | Application (e, f)      -> Application (optim_exp env e, optim_exp env f)
  | Let (x, e, f)           -> Let (x, optim_exp env e, optim_exp env f)

  | New (x, e, Seq (f, g))  -> let l = newref() in (match f with
      | Asg (y, h) when Identifier x = y    -> (match optim_exp env h with
          | Const i   ->
            let v2 = add store (string_of_int l) (Int i);     optim_exp ((x, Ref l)::env) g in
            remove store (string_of_int l);                     New (x, Const i, v2)
          | Bool i    ->
            let v2 = add store (string_of_int l) (Bool i);      optim_exp ((x, Ref l)::env) g in
            remove store (string_of_int l);                     New (x, Bool i, v2)
          | _         -> New (x, optim_exp env e, Seq (optim_exp env f, optim_exp env g)))
      | _   -> New (x, optim_exp env e, Seq (optim_exp env f, optim_exp env g)))

  | e                       -> e

let rec optim_program = function
  | [] -> []
  | Fundef (name, args, body)::ys -> Fundef (name, args, (optim_exp [] body))::optim_program ys
