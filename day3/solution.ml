(* preprossing *)
let inputfile = "input.txt"
let input_str_lists = Lib.read_file inputfile (String.split_on_char ',')

type direction = {x: int ; y : int}
type instruction = {dir: direction ; dist: int}
  
let get_instruction cmd =
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
      let wiring =  board.(!cx).(!cy) in
      (board.(!cx).(!cy) <- match wiring with
                           | No_wires -> ws
                           | Both_wires -> Both_wires
                           | state -> if state == ws then ws else Both_wires) ;
      loop ())
  in loop ()
  
let part1 input =
  let board = Array.make_matrix 100 100 No_wires in

let part2 input  = input

let () =
  let part1result = (part1 input) in
  Format.printf "Answers:@.Solution to part one: %d@.Solution to part two: %d@."
    (part1result.tape.(0)) (part2 input)
