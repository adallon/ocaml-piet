(**
 * This module provides the list of instruction as its main type.
 * It also provides a transition function that computes
 * the instruction from codels
 *
 *)


(**
 * Main type of the module, containing the instructions
 *)
type t = 
  | NoInst    | Push      | Pop (* basic stack *)   
  | Add       | Substract | Multiply | Divide    | Mod (* Basic arithmetic *)
  | Not       | Greater (* Logic  *)    
  | Pointer   | Switch  (* Arrows *)
  | Duplicate | Roll    (* Advanced stack *)  
  | InInt     | InChar  (* io : input  *)  
  | OutInt    | OutChar (* io : output *)


(**
 * Computation of the instruction from the codels
 *
 *)
val transition : bool -> Codel.t -> Codel.t -> t
