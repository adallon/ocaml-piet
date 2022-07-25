module Point = struct
  type t = int * int

  let to_string (x,y) = String.concat "" ["(";string_of_int x;",";string_of_int y;")"]

  let to_point (x,y) = (x,y)
  let x (a,_) = a
  let y (_,b) = b

  let equal (x0,y0) (x1,y1) =  x0 = x1 && y0 = y1
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

  module Relevant = struct
    (* type relevant is used to avoid input confusion.
     * This way the relevant coordinate cannot be confused
      * with any other int *)
    type t = R of int

    let value r = match r with R(n) -> n
    let choose (x,y) = function
    | North | South -> (R y)
    | East  | West  -> (R x)

    let comparison dir x0 x1 =
      (* x0 and x1 are the relevant coordinate *)
      match dir with
      (* x1 is further in dir than x0 *)
      | North | West  -> (value x0) > (value x1) 
      | East  | South -> (value x0) < (value x1)
  end

  let furthest l dir = 
    let rec aux res p0 dir = function
      | [] -> res
      | p1::t ->
        let a0 = Relevant.choose p0 dir in
        let a1 = Relevant.choose p1 dir in
        if a0 = a1 
        then aux (p1::res) p0 dir t
        else if Relevant.comparison dir a0 a1
        then aux [p1] p1 dir t
        else aux res  p0 dir t
    in match l with
    | [] -> assert(false) (* no empty codel block *)
    | a::t -> aux [a] a dir t
end

(*
module type RECTANGLE = sig
  type t
  type elt
  val element_at : t -> Point.t -> elt
  val sizeX : t -> int
  val sizeY : t -> int
  val inside: t -> Point.t -> bool
  val set   : t -> Point.t -> elt -> unit
  val create : elt -> int -> int -> t
  val iter : (Point.t -> elt -> unit) -> t -> unit
end
*)

module Rectangle =
  functor (Elt : Util.Basic) -> 
    struct
      
      type t = Elt.t array array
      type elt = Elt.t
      
      let element_at m p = 
        let x = Point.x p in
        let y = Point.y p in
        m.(x).(y)
      
      let sizeX m = Array.length m
      let sizeY m = Array.length (m.(0)) 
      
      let inside m p = 
        let x = Point.x p in
        let y = Point.y p in
        0 <= x && x < (sizeX m) && 0 <= y && y < (sizeY m)
      
      let set m p e =
        let x = Point.x p in
        let y = Point.y p in
        m.(x).(y) <- e
      
      let create e sizex sizey = 
        let arr_map   = Array.make sizex [||] in
        let rec aux = function
          | 0 -> ()
          | n -> 
              begin 
                arr_map.(n-1)   <- Array.make sizey e ; 
                aux (n-1)
              end
        in aux sizex; arr_map

      let iter f map = 
        let rec apply_f_x x = function
          | 0 -> ()
          | n -> 
            begin 
              f (Point.to_point (x,n-1)) (map.(x).(n-1)) ; 
              apply_f_x x (n-1) 
            end
        in let rec apply_f = function
          | 0 -> ()
          | n -> begin apply_f_x (n-1) (sizeY map) ; apply_f (n-1) end
        in let _ = apply_f (sizeX map) in ()

  end
