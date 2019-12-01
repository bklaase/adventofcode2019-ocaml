(* generic input parsing.. put in separate module *)
let read_file filename = 
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := (int_of_string @@ input_line chan) :: !lines
    done; !lines
  with End_of_file ->
    close_in chan; List.rev !lines 

let input = read_file "input.txt" 

(* solution *)
let calcmodfuel mw = mw / 3 - 2 

let calcmodfuel_includefuel mw =
  let initialfuel = calcmodfuel mw in
  let rec loop total fuel =
    let extrafuel = calcmodfuel fuel in
    if extrafuel <= 0 then total
    else loop (extrafuel+total) extrafuel
  in loop initialfuel initialfuel 

let day1a =
  let addnext a e = a + calcmodfuel e in
  List.fold_left addnext 0 input 

let day1b =
  let addnext a e = a + calcmodfuel_includefuel e in
  List.fold_left addnext 0 input 
