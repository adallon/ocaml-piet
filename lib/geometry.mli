(** This module provides geometric types and functions.
   The geometry of the image-program of Piet is quite simple:
   it is a rectangle graph where codels are vertices.
   There is an edge between codel a and codel b
   if both codels are at manhattan distance 1.
   This module also provides modules 
   for direction and hand choice (aka "codel chooser"). 
 *)

(** Module Point describes the basic definitions on the point level *)
module Point : sig

  (** Basic point type with integer coordinates *)
  type t
    
  (** Converts points as strings *)
  val to_string: t -> string

  (** Convert coordinates as a Point.t element *)
  val to_point : int * int -> t

  (** Returns the first coordinate of a point  *)
  val x : t -> int

  (** Returns the second coordinate of a point  *)
  val y : t -> int

  (** Tells if two codels are equal  *)
  val equal: t -> t -> bool
  
  (** Tells if two codels are close, that is at Manhattan distance 1 *)
  val is_close: t -> t list -> bool
end

(** module Hand represents the values of the "codel chooser"
   and the associated functions. *)
module Hand : sig

  (** The type describe the two possible values of the chooser.
      It does not really makes sense to hide those values
      as we need to access at least one of them
      to initialize the machine and we can get the other one
      with switch. *)
  type t = Left | Right

  (** Converts the value of the codel chooser to string *)
  val to_string: t -> string

  (** Switches between values of the codel chooser *)
  val switch : t -> t
end

module Direction : sig

  (** The type describes the possible values of the direction pointer DP
     The four values are hidden. *)
  type t

  (** Converter of a direction to a string.
     We do not use the conventionnal Left,Right,Down,Up to avoid confusion with the codel chooser values.
     Instead, we use cardinal points (West, East, South, North)
     *)
  val to_string : t -> string

  (**
     Only visible value, used to initialize the DP of the machine.
   *)
  val east : t
  
  (**
     Function used to rotate direction clockwise, 
     a number of time given as a parameter
   *)
  val rotate: t -> int -> t
  
  (**
     Function used to compute the next point in some direction,
     starting from a point given as a parameter.
   *)
  val next_point: Point.t -> t -> Point.t
  
  (**
     Furthest point of a list in some given direction.
     Used to compute the edges of a color block and corners of edges
   *)
  val furthest : Point.t list -> t -> Point.t list
end

  (**
     Functor used to provide a complete geometry over elements.
     Used to describe the image as a rectangle of codels.
     Also used for a module avoiding costly recomputations.
   @param Elt module describing elements that will appear at points of the rectangle
   *)
module Rectangle : functor (Elt : Util.Basic) ->  sig
  
  (**
     Type of the rectangle
   *)
  type t
  
  (**
     Type of the element at each point
   *)
  type elt = Elt.t

  (**
     Function returning the element at a given point.
   *)
  val element_at : t -> Point.t -> elt

  (**
     Function returning the width of the rectangle
   *)
  val sizeX : t -> int

  (**
     Function returning the height of the rectangle
   *)
  val sizeY : t -> int

  (**
     Function telling whether a point given as a parameter is inside
     the rectangle
   *)
  val inside: t -> Point.t -> bool

  (**
     Function allowing to put some elt value at some point
   *)
  val set   : t -> Point.t -> elt -> unit

  (**
     Function creating a rectangle of element of size given in parameter
   *)
  val create : elt -> int -> int -> t

  (**
     Function allowing to iterate a unitary function on elements
     and points on all elements of a rectangle
   *)
  val iter : (Point.t -> elt -> unit) -> t -> unit
end

