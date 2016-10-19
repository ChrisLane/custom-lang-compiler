(* Print command usage information *)
let print_help () =
  print_endline ("\nPlease use: './main <task> <file>'\n");
  print_endline ("Available tasks:");
  print_endline ("\tparse - Parse a source file");
  print_endline ("\tevaluate - Evaluate a source file");
  print_endline ""

(* Run the appropriate command *)
let run_task task file = match task with
  | "parse"     -> Print.parse_file file
  | "evaluate"  -> Print.eval_file file
  | _           -> print_endline "\nInvalid task."; print_help(); exit 1

(* If more/less than 2 arguement is given, print proper command usage *)
let _ = match Array.length Sys.argv with
  | 3 -> run_task Sys.argv.(1) Sys.argv.(2)
  | _ -> print_help(); exit 1
