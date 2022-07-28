

(** Module Point describes the basic definitions on the point level *)
module Point = struct

  (** Basic point type with integer coordinates *)
  type t = int * int

  (** Converts points as strings *)
  let to_string (x,y) = String.concat "" ["(";string_of_int x;",";string_of_int y;")"]

  (** Convert coordinates of type int*int as a Point.t element *)
  let to_point (x,y) = (x,y)

  (** Returns the first coordinate of a point  *)
  let x (a,_) = a

  (** Returns the second coordinate of a point  *)
  let y (_,b) = b

  (** Tells if two codels are equal  *)
  let equal (x0,y0) (x1,y1) =  x0 = x1 && y0 = y1
  
  (** Tells if two codels are close, that is at Manhattan distance 1 *)
  let rec is_close (x,y) = function
    | [] -> false
    | (x1,y1)::t -> 
      (abs(x1-x) < 2 &&  y1 = y) || 
      (abs(y1-y) < 2 &&  x1 = x) ||
      (is_close (x,y) t) 
end

(** module Hand represents the values of the "codel chooser"
 * and the associated functions. *)
module Hand = struct

  (** The type describe the two possible values of the chooser. *)
  type t = Left | Right

  (** Converts the value of the codel chooser to string *)
  let to_string = function
    | Left  -> "left"
    | Right -> "right"

  (** Switches between values of the codel chooser *)
  let switch = function
    | Left  -> Right
    | Right -> Left
end


module Direction = struct

  (** The type describes the possible values of the direction pointer DP
   * The four values are hidden.
   * We do not use the conventionnal Left,Right,Down,Up to avoid confusion with the codel chooser values.
   * Instead, we use cardinal points (West, East, South, North)
   * *)
  type t = North | South | West | East

  (** Converter of a direction to a string.
   * We do not use the conventionnal Left,Right,Down,Up to avoid confusion with the codel chooser values.
   * Instead, we use cardinal points (West, East, South, North)
   * *)
  let to_string = function
    | North -> "North"
    | East  -> "East"
    | South -> "South"
    | West  -> "West"

  (**
   * Only visible value, used to initialize the DP of the machine.
   *)
  let east  = East

  (** Function converting integer to directions.
   * Converse function of to_int
   * Used for rotations and hidden outside of the Geometry module.*)
  let rec of_int = function
    | 0 -> North
    | 1 -> East
    | 2 -> South
    | 3 -> West
    | n -> of_int ((4+(n mod 4)) mod 4)

  (** Function converting directions to integers.
   * Converse function of of_int.
   * Used for rotations and hidden outside of the Geometry module.*)
  let to_int = function
    | North -> 0
    | East  -> 1
    | South -> 2
    | West  -> 3

  (**
   * Function used to rotate direction clockwise, 
   * a number of time given as a parameter
   *)
  let rotate dir n =
    let k = to_int dir in of_int (k+n)
 
  (**
   * Function used to compute the next point in some direction,
   * starting from a point given as a parameter.
   *)
  let next_point (x,y)  = function
    | North -> (x,y-1)
    | South -> (x,y+1)
    | West  -> (x-1,y)
    | East  -> (x+1,y)

  (**
   * module relevant is used to avoid input confusion.
   * Only relevant coordinates, that is results of choose,
   * can be compared by the comparison operation.
   *)
  module Relevant = struct

    (**
     * Type of relevant coordinates.
     * It is an int describing the value of the x coordinate
     * or of the y coordinate depending on the direction
     * (x coordinate for West/East and y coordinate for North/South)
     *)
    type t = R of int

    (**
     * Retrieves the original value of the coordinate
     *)
    let value r = match r with R(n) -> n

    (**
     * Chooses the relevant coordinate according to the direction
     *)
    let choose (x,y) = function
    | North | South -> (R y)
    | East  | West  -> (R x)

    (**
     * Tells whether or not x1 is further than x0 in direction dir
     * according to the value of the relevant coordinate.
     * The upper left handside directions (North, West) 
     * should have smaller coordinates,
     * while the lower right handisde directions (South, East)
     * should have larger coordinates
     @param dir direction under consideration
     @param x0 first value to compare
     @param x1 second value to compare
     *)
    let comparison dir x0 x1 =
      match dir with
      | North | West  -> (value x0) > (value x1) 
      | East  | South -> (value x0) < (value x1)
  end

  (** 
   * Function computing the list of (equally) furthest points in direction dir inside list l
   * @param l list of points 
   * @param dir direction under consideration
   *)
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

  (**
   * Functor used to provide a complete geometry over elements.
   * Used to describe the image as a rectangle of codels.
   * Also used for a module avoiding costly recomputations.
   @param Elt module describing elements that will appear at points of the rectangle
   *)
module Rectangle =
  functor (Elt : Util.Basic) -> 
    struct
  
  (**
   * Type of the rectangle
   *)
      type t = Elt.t array array
  
  (**
   * Type of the element at each point
   *)
      type elt = Elt.t
      
  (**
   * Function returning the element at a given point.
   *)
      let element_at m p = 
        let x = Point.x p in
        let y = Point.y p in
        m.(x).(y)
      
  (**
   * Function returning the width of the rectangle
   *)
      let sizeX m = Array.length m

  (**
   * Function returning the height of the rectangle
   *)
      let sizeY m = Array.length (m.(0)) 
      

  (**
   * Function telling whether a point given as a parameter is inside
   * the rectangle
   * @param m the complete rectangle
   * @param p the point
   *)
      let inside m p = 
        let x = Point.x p in
        let y = Point.y p in
        0 <= x && x < (sizeX m) && 0 <= y && y < (sizeY m)
      
  (**
   * Function allowing to put some elt value at some point
   * @param m the complete rectangle
   * @param p the point where the new value must be set 
   * @param e value to set at point p of rectangle m
   *)
      let set m p e =
        let x = Point.x p in
        let y = Point.y p in
        m.(x).(y) <- e
      
  (**
   * Function creating a rectangle of element of size given in parameter
   * @param e initilization value of the rectangle
   * @param sizex width  of the rectangle
   * @param sizey height of the rectangle
   *)
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

  (**
   * Function iterating f on elements and points of rectangle map
   * @param f function Point.t -> elt -> unit applying a unitary operation to each point according to the value
   * @param map rectangle on which f must be iterated
   *)
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
