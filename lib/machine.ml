type direction = North | South | West | East
type chooser   = Left  | Right

let next_point (x,y)  = function
  | North -> (x,y+1)
  | South -> (x,y-1)
  | West  -> (x-1,y)
  | East  -> (x+1,y)

type t = Stack.t * direction * chooser * (int * int) * int
(* Stack, Direction Pointer DP, Codel Chooser CC, Current coordinates, size *)



