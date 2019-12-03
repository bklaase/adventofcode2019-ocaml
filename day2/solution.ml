let inputfile = "day2/input.txt"
let input = [| 1;0;0;3;1;1;2;3;1;3;4;3;1;5;0;3;2;1;13;19;2;9;19;23;1;23;6;27;1;13;27;31;1;31;10;35;1;9;35;39;1;39;9;43;2;6;43;47;1;47;5;51;2;10;51;55;1;6;55;59;2;13;59;63;2;13;63;67;1;6;67;71;1;71;5;75;2;75;6;79;1;5;79;83;1;83;6;87;2;10;87;91;1;9;91;95;1;6;95;99;1;99;6;103;2;103;9;107;2;107;10;111;1;5;111;115;1;115;6;119;2;6;119;123;1;10;123;127;1;127;5;131;1;131;2;135;1;135;5;0;99;2;0;14;0 |]
let testinput1 = [| 1;1;1;4;99;5;6;0;99 |]

(* solution *)
type progstate = {tape : int array ; mutable pos : int ;}
type instruction = {op: int ; a: int ; b : int; target : int }
let getop n = match n with
  | 1 -> ( + )
  | 2 -> ( * )
  | _ -> failwith "unknown op"

let tape_deref state i =
  state.tape.(state.tape.(state.pos+i))

let getinstruction state =
  let op = state.tape.(state.pos) in
  let a = tape_deref state 1 in
  let b = tape_deref state 2 in
  let target = state.tape.(state.pos+3) in
  {op=op; a=a; b=b; target=target}

let process_tape input noun verb =
  let initstate = {tape=Array.copy input; pos=0} in
  initstate.tape.(1) <- noun;
  initstate.tape.(2) <- verb;
  let rec step state =
    if state.tape.(state.pos) == 99 then
      state
    else (
      let i = getinstruction state in
      state.tape.(i.target) <-
        (getop i.op)  i.a i.b;
      state.pos <- state.pos+4;
      step state
    )
  in
  step initstate

let part1 input =
  let result = process_tape input 12 2 in
  result

let part2 input  =
  let rec loop n v =
    let result = process_tape input n v in
    if result.tape.(0)  == 19690720  then
      100 * n + v
    else if (n < 99) then loop (n+1) v else loop 0 (v+1)
  in loop 0 0

let () =
  let part1result = (part1 input) in
  Format.printf "Answers:@.Solution to part one: %d@.Solution to part two: %d@."
    (part1result.tape.(0)) (part2 input)
