let help () = print_endline ("Please use: './main <file>\n")

(* If more/less than 1 arguement is given, print proper command usage *)
let _ =
  if Array.length (Sys.argv) != 2
  then (help (); exit 1)
  else Print.parse_file (Sys.argv.(1))

