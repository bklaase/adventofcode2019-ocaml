(* to use tuareg repl, run these
#mod_use "../lib/lib.ml" ;;
let inputfile = "input.txt" ;;
 *)

(* generic input parsing *)
let read_file filename func = 
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := (func  @@ input_line chan) :: !lines
    done; !lines
  with End_of_file ->
    close_in chan; List.rev !lines 


let print_results r1 r2 =                       
  Format.printf "Answers:@.Solution to part one: %s@.Solution to part two: %s@."
    r1 r2
