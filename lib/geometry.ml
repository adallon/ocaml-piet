module Point = struct
  type t = int * int

  let to_string (x,y) = String.concat "" ["(";string_of_int x;",";string_of_int y;")"]

  let to_point (x,y) = (x,y)
  let x (a,_) = a
  let y (_,b) = b


  let rec is_close (x,y) = function
    | [] -> false
    | (x1,y1)::t -> 
      (abs(x1-x) < 2 &&  y1 = y) || 
      (abs(y1-y) < 2 &&  x1 = x) ||
      (is_close (x,y) t) 
end

module Hand = struct
  type t = Left | Right

  let to_string = function
    | Left  -> "left"
    | Right -> "right"

  let switch = function
    | Left  -> Right
    | Right -> Left
end


module Direction = struct
  type t = North | South | West | East

  let to_string = function
    | North -> "North"
    | East  -> "East"
    | South -> "South"
    | West  -> "West"

  let east  = East

  let rec of_int = function
    | 0 -> North
    | 1 -> East
    | 2 -> South
    | 3 -> West
    | n -> of_int ((4+(n mod 4)) mod 4)

  let to_int = function
    | North -> 0
    | East  -> 1
    | South -> 2
    | West  -> 3

  let rotate dir n =
    let k = to_int dir in of_int (k+n)
  
  let next_point (x,y)  = function
    | North -> (x,y-1)
    | South -> (x,y+1)
    | West  -> (x-1,y)
    | East  -> (x+1,y)

  type relevant = R of int
  (* type relevant is used to avoid input confusion.
   * This way the relevant coordinate cannot be confused
   * with any other int *)

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

  let dir_hand_order dir hand =
    let dir0 = dir in
    let dir1 = rotate dir0 1 in
    let dir2 = rotate dir1 1 in
    let dir3 = rotate dir2 1 in
    let hand0 = hand in
    let hand1 = Hand.switch hand in
    [dir0,hand0; dir0,hand1; dir1,hand1; dir1,hand0;
     dir2,hand0; dir2,hand1; dir3,hand1; dir3,hand0]
end
