type direction = North | South | West | East

type hand      = Left  | Right

let init_dir  = East
let init_hand = Left

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

let hand_to_int = function
  | Left  -> -1
  | Right -> 1

let rotate dir n =
  let k = direction_to_int dir in
  direction_from_int (k+n)

type relevant = R of int
let value r = match r with R(n) -> n
let relevant_coord (x,y) = function
  | North | South -> (R y)
  | East  | West  -> (R x)

let comparison dir x0 x1 =
  (* x0 and x1 are the relevant coordinate *)
  match dir with
  (* x1 is further in dir than x0 *)
  | North | West  -> (value x0) > (value x1) 
  | East  | South -> (value x0) < (value x1)

let furthest l dir = 
  let rec aux res cur_coord dir = function
    | [] -> res
    | (x1,y1)::t ->
        let a0 = relevant_coord cur_coord dir in
        let a1 = relevant_coord (x1,y1)   dir in
        if a0 = a1 then aux ((x1,y1)::res) cur_coord dir t
        else if comparison dir a0 a1
        then aux [x1,y1] (x1,y1) dir t
        else aux res cur_coord dir t
  in match l with
  | [] -> assert(false) (* no empty codel block *)
  | a::t -> aux [a] a dir t


let hand_switch = function
  | Left  -> Right
  | Right -> Left

let rotate_hand dir hand = 
  rotate dir (hand_to_int hand)

let next_point (x,y)  = function
  | North -> (x,y+1)
  | South -> (x,y-1)
  | West  -> (x-1,y)
  | East  -> (x+1,y)

let dir_hand_order dir hand =
  let dir0 = dir in
  let dir1 = rotate dir0 1 in
  let dir2 = rotate dir1 1 in
  let dir3 = rotate dir2 1 in
  let hand0 = hand in
  let hand1 = hand_switch hand in
  [dir0,hand0; dir0,hand1; dir1,hand1; dir1,hand0;
   dir2,hand0; dir2,hand1; dir3,hand1; dir3,hand0]
