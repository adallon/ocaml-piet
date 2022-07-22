open Instructions

type t = Stack.t * Direction.direction * Direction.hand
(* Stack, Direction Pointer DP, Codel Chooser CC *)

let init_machine = Stack.empty,Direction.init_dir,Direction.init_hand

let get_direction ((a,b,c):t) = b
let get_hand      ((a,b,c):t) = c
let set d h ((a,b,c):t) = a,d,h

let next_state machine prev_blocksize = 
  let (st,dp,cc) = machine in function
  | Push -> (Stack.push prev_blocksize st,dp,cc)
  | Pop  -> (Stack.pop st,dp,cc)
  | Add  -> (Stack.add st,dp,cc)
  | Substract  -> (Stack.subst st,dp,cc)
  | Multiply   -> (Stack.mult  st,dp,cc)
  | Divide     -> (Stack.div   st,dp,cc)
  | Mod        -> (Stack.modulo st,dp,cc)
  | Not        -> (Stack.not st,dp,cc)
  | Greater    -> (Stack.greater st,dp,cc)
  | Pointer    -> 
      let n0,st1 = Stack.get st in
      begin match n0 with
      | None    -> (st,dp,cc)
      | Some(n) -> (st1,Direction.rotate dp n,cc)
      end
  | Switch     -> 
      let n0,st1 = Stack.get st in
      begin match n0 with
      | None    -> (st,dp,cc)
      | Some(n) -> 
          (st1,dp, if n mod 2 = 0 then cc else Direction.hand_switch cc)
      end
  | Duplicate  -> (Stack.duplicate st,dp,cc)
  | Roll       -> (Stack.roll st,dp,cc)
  | InInt      -> (Stack.inint st,dp,cc)
  | InChar     -> (Stack.inchar st,dp,cc)
  | OutInt     -> (Stack.outint st,dp,cc)
  | OutChar    -> (Stack.outchar st,dp,cc)

