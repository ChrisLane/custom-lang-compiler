(* Print command usage information *)
let print_help () =
  print_endline ("\nPlease use: './main <task> <file>'\n");
  print_endline ("Available tasks:");
  print_endline ("\tparse - Parse a source file");
  print_endline ("\tevaluate - Evaluate a source file");
  print_endline ""

(* Run the appropriate command *)
let run_task task optimise file = match task with
  | "parse"     -> (match optimise with
      | true    -> Print.parse_file_optimised file
      | false   -> Print.parse_file file)
  | "evaluate"  -> (match optimise with
      | true    -> Print.eval_file_optimised file
      | false   -> Print.eval_file file)
  | _           -> print_endline "\nInvalid task."; print_help(); exit 1

(* If more/less than 2 arguement is given, print proper command usage *)
let _ = match Array.length Sys.argv with
  | 3 -> run_task Sys.argv.(1) false Sys.argv.(2)
  | 4 -> (match Sys.argv.(2) with
    | "-o"  -> run_task Sys.argv.(1) true Sys.argv.(3)
    | _     -> print_endline "\nInvalid flag."; print_help(); exit 1)
  | _ -> print_help(); exit 1
