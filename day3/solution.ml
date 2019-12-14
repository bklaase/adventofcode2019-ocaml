Printexc.record_backtrace true ;;
open Base ;;
(* preprossing *)
let inputfile = "day3/input.txt"
let input_str_lists = Lib.read_file inputfile (String.split ~on:',')

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
  let am = String.sub cmd ~pos:1 ~len:((String.length cmd) - 1) in
  {dir= d; dist= int_of_string am}

let input = List.map ~f:(fun cmds ->
                (List.map ~f:get_instruction cmds ))
              input_str_lists

(* solution *)
type wires_state = No_wires | Wire_one | Wire_two | Both_wires
let wires_state_str = function
  | No_wires -> "no wires" | Wire_one -> "wire one"
  | Wire_two -> "wire two" | Both_wires -> "both"

let board : ((int * int),wires_state) Hashtbl.t = Hashtbl.Poly.create ()

let update_board_with_ws board pos ws =
  let wiring = Hashtbl.find board pos in
  Hashtbl.add board ~key:pos ~data:(match wiring with
    | None -> ws
    | Some Both_wires -> Both_wires
    | Some state -> if phys_equal state ws then ws else Both_wires)

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
      ignore @@ update_board_with_ws board (!cx,!cy) ws;
      loop ())
  in loop ()

(* let part1 input = *)
