open Ast

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

let rec optim_exp = function
  | Empty -> Empty
  | Const e -> Const e
  | Identifier e -> Identifier e
  | Deref e -> Deref (optim_exp e)
  | While (e, f) -> While (optim_exp e, optim_exp f)
  | If (e, f, g) -> (match optim_exp e with
    | Bool true  -> optim_exp f
    | Bool false -> optim_exp g
    | _     -> If (optim_exp e, optim_exp f, optim_exp g))
  | Operator (op, e, f) -> optim_operator op e f
  | Asg (e, f) -> Asg (optim_exp e, optim_exp f)
  | Seq (e, f) -> Seq (optim_exp e, optim_exp f)
  | Print e -> Print (optim_exp e)
  | Application (e, f) -> Application (optim_exp e, optim_exp f)
  | Let (x, e, f) -> Let (x, optim_exp e, optim_exp f)
  | New (x, e, Seq (f, g)) -> (match optim_exp f with
      | Asg (y, h) when Identifier x = y -> (match optim_exp h with
        | Const i -> New (x, Const i, optim_exp g)
        | Bool i -> New (x, Bool i, optim_exp g)
        | _ -> New (x, e, Seq (f, g)))
      | _ -> New (x, optim_exp e, Seq (optim_exp f, optim_exp g)))
  | e -> e

let optim_program = function
  | Fundef (name, args, body) -> Fundef (name, args, (optim_exp body))
