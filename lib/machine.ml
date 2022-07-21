open Instructions

type direction = North | South | West | East

type chooser   = Left  | Right

let rec direction_from_int = function
  | 0 -> North
  | 1 -> East
  | 2 -> South
  | 3 -> West
  | n -> direction_from_int ((4+(n mod 4)) mod 4)

let direction_to_int = function
  | North -> 0
  | East  -> 1
  | South -> 2
  | West  -> 3

let direction_rotate dir n =
  let k = direction_to_int dir in
  direction_from_int (k+n)

let chooser_switch = function
  | Left  -> Right
  | Right -> Left


let next_point (x,y)  = function
  | North -> (x,y+1)
  | South -> (x,y-1)
  | West  -> (x-1,y)
  | East  -> (x+1,y)

let next_point_chooser_dir (x,y) dir = function
  | Right -> next_point (x,y) (direction_rotate dir  1)
  | Left  -> next_point (x,y) (direction_rotate dir (-1))


type t = Stack.t * direction * chooser
(* Stack, Direction Pointer DP, Codel Chooser CC *)

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
      | Some(n) -> (st1,direction_rotate dp n,cc)
      end
  | Switch     -> 
      let n0,st1 = Stack.get st in
      begin match n0 with
      | None    -> (st,dp,cc)
      | Some(n) -> 
          (st1,dp, if n mod 2 = 0 then cc else chooser_switch cc)
      end
  | Duplicate  -> (Stack.duplicate st,dp,cc)
  | Roll       -> (Stack.roll st,dp,cc)
  | InInt      -> (Stack.inint st,dp,cc)
  | InChar     -> (Stack.inchar st,dp,cc)
  | OutInt     -> (Stack.outint st,dp,cc)
  | OutChar    -> (Stack.outchar st,dp,cc)

