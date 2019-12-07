Printexc.record_backtrace true ;;
(* preprossing *)
let input = (138241,674034)

(* solution *)
let valid_pass pass =
  let double_encountered = ref false in
  let rec loop i j =
    if j > 5 then !double_encountered
    else if pass.(i) == pass.(j) then
      (double_encountered := true; loop (i+1) (j+1))
    else if pass.(i) > pass.(j) then false
    else loop (i+1) (j+1);
  in loop 0 1

let valid_pass2 pass =
  let double_encountered = ref false in
  let rec loop i j =
    if j > 5 then !double_encountered
    else (
      let (a,b) = pass.(i),pass.(j) in
      
      if a > b then false
      else (
        if a == b then ( 
          let count = Array.fold_left (fun c e -> if e==a then c+1 else c) 0  pass in
          (if count == 2 then double_encountered := true) 
        );
        loop (i+1) (j+1)
      )
    )
  in loop 0 1

let int_array_of_int i =
  i |> string_of_int |> String.to_seq
  |> Seq.map (fun c -> ((int_of_char c) - 48))
  |> Array.of_seq  
               
let proccess_options start stop check_method =
  let count = ref 0 in
  let rec loop i:int = match i with
    | i when (i > stop ) -> !count
    | i -> if check_method @@ int_array_of_int i then
             (count := !count+1; loop (i+1))
           else loop (i+1)
  in loop start

let part1 (start,stop) =
  proccess_options start stop valid_pass

let part2 (start,stop) =
  proccess_options start stop valid_pass2
  
(* print outcome *)  
let () =
  Lib.print_results (string_of_int @@ part1 input) (string_of_int @@ part2 input)
