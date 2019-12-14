let inputfile = "day1/input.txt"
let input = Lib.read_file inputfile int_of_string

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

let () =
  Lib.print_results
    ( string_of_int day1a )
    ( string_of_int day1b )
