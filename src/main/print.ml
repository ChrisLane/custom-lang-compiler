open Ast

(* Return string values for data types *)
let dtype_string = function
  | Int i   -> string_of_int i
  | Ref i   -> failwith "Reached point of printing a ref"
  | Bool b  -> string_of_bool b
  | Var v   -> v
  | Unit    -> " "

(* Return string values for operators *)
let opcode_string = function
  | Plus    -> "Plus"
  | Minus   -> "Minus"
  | Times   -> "Times"
  | Divide  -> "Divide"
  | Leq     -> "Leq"
  | Geq     -> "Geq"
  | Equal   -> "Equal"
  | Noteq   -> "Noteq"
  | And     -> "And"
  | Or      -> "Or"
  | Not     -> "Not"

(* Build a given indentation level string *)
let rec indentrec i s = match i with
  | 0 -> s ^ ""
  | i -> indentrec (i-1) (s ^ "    ")

(* Return a given indentation level's string (less args) *)
let indent i = indentrec i ""

(* Return string values for expressions*)
let rec exp_string e i = match e with
  | Seq (e, f)                  -> exp_string e i ^ "; \n" ^ exp_string f i
  | Empty                       -> (indent i) ^ "empty "
  | Readint                     -> (indent i) ^ "Readint () "
  | Identifier s                -> (indent i) ^ "Identifier \"" ^ s ^ "\" "
  | Const n                     -> (indent i) ^ "Const "                    ^ string_of_int n   ^ " "
  | Bool n                      -> (indent i) ^ "Bool "                     ^ string_of_bool n  ^ " "
  | If (e, f, g)                -> (indent i) ^ "If ( "                     ^ exp_string e 0    ^ ") { \n"      ^ exp_string f (i+1) ^ "\n" ^ indent i          ^ "} Else { \n" ^ exp_string g (i+1) ^ "\n" ^ indent i ^ "} "
  | While (e, f)                -> (indent i) ^ "While ( "                  ^ exp_string e 0    ^ ") { \n"      ^ exp_string f (i+1) ^ "\n" ^ indent i          ^ "} "
  | Let (s, e, f)               -> (indent i) ^ "Let ( \"" ^ s ^ "\" = "    ^ exp_string e 0    ^ ") In { \n"   ^ exp_string f (i+1) ^ "\n" ^ indent i          ^ "} "
  | New (s, e, f)               -> (indent i) ^ "New ( \"" ^ s ^ "\" = "    ^ exp_string e 0    ^ ") In { \n"   ^ exp_string f (i+1) ^ "\n" ^ indent i          ^ "} "
  | Asg (e, f)                  -> (indent i) ^ "Asg ( "                    ^ exp_string e 0    ^ ":= "         ^ exp_string f 0                                ^ ") "
  | Deref e                     -> (indent i) ^ "Deref ( "                  ^ exp_string e 0    ^ ") "
  | Operator (Not, Empty, e)    -> (indent i) ^ "Operator ( Not, "          ^ exp_string e 0    ^ ") "
  | Operator (op, e, f)         -> (indent i) ^ "Operator ( "               ^ opcode_string op  ^ ", "          ^ exp_string e 0    ^ ", "  ^ exp_string f 0    ^ ") "
  | Print e                     -> (indent i) ^ "Print ( "                  ^ exp_string e 0    ^ ") "
  | Application (e, Seq (a, b)) -> (indent i) ^ "Application ( "            ^ exp_string e 0    ^ "( "          ^ applicationseq ( Seq (a, b) ) 0               ^ ") ) "
  | Application (e, f)          -> (indent i) ^ "Application ( "            ^ exp_string e 0    ^ "( "          ^ exp_string f 0                                ^ ") ) "

(* Seperate string rule for the Seq expression within application parameters.
   This ensures that parameters are not splt onto seperate lines *)
and applicationseq e i = match e with
  | Seq (e, f)  -> exp_string e i ^ ", " ^ exp_string f i
  | _           -> exp_string e i

(* Return the string of a function *)
let function_string = function
  | Fundef (name, args, body) -> "\n" ^ name ^ " ( " ^ (String.concat ", " args)  ^ " ) { \n" ^ exp_string body 1 ^ "\n}\n"

(* Parse and then print if successful *)
let parse_file filename = open_in filename
                          |> Lexing.from_channel
                          |> Error.parse_with_error
                          |> List.map function_string
                          |> String.concat ""
                          |> print_string
                          |> print_newline

(* Parse, optimise and then print if successful *)
let parse_file_optimised filename = open_in filename
                          |> Lexing.from_channel
                          |> Error.parse_with_error
                          |> Optim.optim_program
                          |> List.map function_string
                          |> String.concat ""
                          |> print_string
                          |> print_newline


(* Parse, evaluate and print a return value if successful *)
let eval_file filename = open_in filename
                         |> Lexing.from_channel
                         |> Error.parse_with_error
                         |> List.map Eval.eval_program
                         |> List.map dtype_string
                         |> String.concat ""
                         |> print_string
                         |> print_newline

(* Parse, optimise, evaluate and print a return value if successful *)
let eval_file_optimised filename = open_in filename
                         |> Lexing.from_channel
                         |> Error.parse_with_error
                         |> Optim.optim_program
                         |> List.map Eval.eval_program
                         |> List.map dtype_string
                         |> String.concat ""
                         |> print_string
                         |> print_newline
