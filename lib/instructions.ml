type t = 
  | NoInst    | Push      | Pop (* basic stack *)   
  | Add       | Substract | Multiply | Divide    | Mod (* Basic arithmetic *)
  | Not       | Greater (* Logic  *)    
  | Pointer   | Switch  (* Arrows *)
  | Duplicate | Roll    (* Advanced stack *)  
  | InInt     | InChar  (* io : input  *)  
  | OutInt    | OutChar (* io : output *)

let to_instr t =
  match t with
  | (0,0) -> NoInst
  | (0,1) -> Push
  | (0,2) -> Pop
  
  | (1,0) -> Add
  | (1,1) -> Substract
  | (1,2) -> Multiply

  | (2,0) -> Divide
  | (2,1) -> Mod
  | (2,2) -> Not

  | (3,0) -> Greater
  | (3,1) -> Pointer
  | (3,2) -> Switch

  | (4,0) -> Duplicate
  | (4,1) -> Roll
  | (4,2) -> InInt

  | (5,0) -> InChar
  | (5,1) -> OutInt
  | (5,2) -> OutChar

  | _ -> assert(false) (* should never occur *)

let transition wh c0 c1 = 
  if wh 
  then
    let _ =
      Util.print_endline 1 "  Passing through a white codel.";
      Util.print_endline 1 "  No instruction executed.";
    in to_instr (0,0)
  else to_instr (Codel.diff c0 c1)
