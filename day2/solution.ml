let inputfile = "day2/input.txt"
let input = [| 1;0;0;3;1;1;2;3;1;3;4;3;1;5;0;3;2;1;13;19;2;9;19;23;1;23;6;27;1;13;27;31;1;31;10;35;1;9;35;39;1;39;9;43;2;6;43;47;1;47;5;51;2;10;51;55;1;6;55;59;2;13;59;63;2;13;63;67;1;6;67;71;1;71;5;75;2;75;6;79;1;5;79;83;1;83;6;87;2;10;87;91;1;9;91;95;1;6;95;99;1;99;6;103;2;103;9;107;2;107;10;111;1;5;111;115;1;115;6;119;2;6;119;123;1;10;123;127;1;127;5;131;1;131;2;135;1;135;5;0;99;2;0;14;0 |]


(* solution *)
type progstate = {tape : int array ; pos : int ;}
type instruction = {op: int -> int -> int ;
                    a: int ; b : int; target : int }
let getop n = match n with
  | 1 -> ( + )
  | 2 -> ( * )
  | _ -> failwith "unknown op"
       
let getinstruction state =
  let a = state.pos+1 in
  let b = state.pos+2 in
  let op = getop state.pos in
  let target = state.pos+3 in
  {op=op; a=a; b=b; target=target}
  
let part1 input  =
  let initstate = {tape=input; pos=0} in
  let step state =
    let i = getinstruction state in
    state.tape.(i.target) <-
      i.op state.tape.(i.a) state.tape.(i.b)
           (* todo break on 99 *)
           (* todo set new pos and recur *)
  in
  step initstate
          


let part2 input  = failwith "not implemented"


let () =
  Format.printf "Answers:
                 Solution to part one: %d
                 Solution to part two: %d@."
    (part1 input) (part2 input)
