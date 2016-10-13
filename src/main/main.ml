
let help () = print_endline ("Please use: './main <file>\n")

let _ =
  if Array.length (Sys.argv) != 2
  then (help (); exit 1)
  else Print.parse_file (Sys.argv.(1))

