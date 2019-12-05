Printexc.record_backtrace true
(* preprossing *)
let inputfile = "day3/input.txt"
let input_str_lists = Lib.read_file inputfile (String.split_on_char ',')

type direction = {x: int ; y : int}
type instruction = {dir: direction ; dist: int}
  
let get_instruction cmd =
  print_endline @@ "str: " ^ cmd;
  let d = match (String.get cmd 0) with
    | 'D' -> {x =  0; y = -1}
    | 'U' -> {x =  0; y =  1}
    | 'L' -> {x = -1; y =  0}
    | 'R' -> {x =  1; y =  0}
    | _   -> failwith "unknown direction"
  in
  let am = String.sub cmd 1 ((String.length cmd) - 1) in
  {dir= d; dist= int_of_string am}
  
let input = List.map (fun cmds ->
                (List.map get_instruction cmds ))
              input_str_lists

(* solution *)
type wires_state = No_wires | Wire_one | Wire_two | Both_wires
let wires_state_str = function
  | No_wires -> "no wires" | Wire_one -> "wire one"
  | Wire_two -> "wire two" | Both_wires -> "both"

let board : ((int * int),wires_state) Hashtbl.t = Hashtbl.create 1000     

let update_board_with_ws board pos ws =
  let wiring = Hashtbl.find board pos in
  Hashtbl.replace board pos (match wiring with
    | No_wires -> ws
    | Both_wires -> Both_wires
    | state -> if state == ws then ws else Both_wires)
  
let apply_instruction_to_board_pos instr board (x,y) ws =
  let (cx,cy) = (ref x, ref y) in
  let (tx,ty) = (x + instr.dir.x * instr.dist,
                 y + instr.dir.y * instr.dist) in
  let rec loop () =
    if (!cx == tx) && (!cy == ty) then
     board
    else (
      cx := !cx + instr.dir.x;
      cy := !cy + instr.dir.y;
      update_board_with_ws board (!cx,!cy) ws;
      loop ())
  in loop ()
  
let part1 () =

let part2 input  = input

let () =
  let part1result = (part1 ()) in
  Format.printf "Answers:@.Solution to part one: %s@.Solution to part two: ...d@."
    (wires_state_str part1result.(0).(4)) 
