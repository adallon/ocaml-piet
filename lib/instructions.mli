type t = 
  | NoInst    | Push      | Pop (* basic stack *)   
  | Add       | Substract | Multiply | Divide    | Mod (* Basic arithmetic *)
  | Not       | Greater (* Logic  *)    
  | Pointer   | Switch  (* Arrows *)
  | Duplicate | Roll    (* Advanced stack *)  
  | InInt     | InChar  (* io : input  *)  
  | OutInt    | OutChar (* io : output *)

val transition : bool -> Codel.t -> Codel.t -> t
