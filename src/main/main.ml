(* Print command usage information *)
let print_help () =
  print_endline ("\n================================================");
  print_endline ("Please use: './main <task> <flag> <file>'\n");
  print_endline ("Available flags:");
  print_endline ("\t '-o' - Optimise the parsed program.");
  print_endline ("\nAvailable tasks:");
  print_endline ("\t'parse' - Parse a source file.");
  print_endline ("\t'evaluate' - Evaluate a source file.");
  print_endline ("\t'interpret' - Interpret a source file.");
  print_endline ("\t'codegen' - Generate code from a source file.");
  print_endline ("==================================================");
  print_endline ""

(* Run the appropriate command *)
let run_task task optimise file = match task with
  | "parse"     -> (match optimise with
      | true    -> Print.parse_file_optimised file
      | false   -> Print.parse_file file)
  | "evaluate"  -> (match optimise with
      | true    -> Print.eval_file_optimised file
      | false   -> Print.eval_file file)
  | "interpret" -> (match optimise with
      | true    -> Print.interpret_file_optimised file
      | false   -> Print.interpret_file file)
  | "codegen"   -> (match optimise with
      | true    -> Print.codegen_file_optimised file
      | false   -> Print.codegen_file file)
  | _           -> print_endline "\nInvalid task."; print_help(); exit 1

(* If more/less than 2 arguement is given, print proper command usage *)
let _ = match Array.length Sys.argv with
  | 3 -> run_task Sys.argv.(1) false Sys.argv.(2)
  | 4 -> (match Sys.argv.(2) with
    | "-o"  -> run_task Sys.argv.(1) true Sys.argv.(3)
    | _     -> print_endline "\nInvalid flag."; print_help(); exit 1)
  | _ -> print_help(); exit 1
