open Base;;
(* generic input parsing *)
let read_file filename func = 
  let lines = Stdio.In_channel.read_lines filename in
  List.map ~f:func lines


let print_results r1 r2 =                       
  Stdio.printf
"Answers:
Solution to part1: %s
Solution to part2: %s\n"
    r1 r2

let rec cartesian_prod (ls: 'a list list) : 'a list list
  = match ls with
  | [] -> [[]]
  | h :: t ->
     let rest = cartesian_prod t in
     List.concat( List.map ~f:(fun i ->
                      List.map ~f:(fun r -> i :: r) rest )
                    h )
       
