module Point : sig
  type t
  val to_point : int * int -> t
  val x : t -> int
  val y : t -> int
  val is_close: t -> t list -> bool
  val to_string: t -> string
end

module Hand : sig
  type t = Left | Right
  val to_string: t -> string
  val switch : t -> t
end

module Direction : sig
  type t
  val to_string : t -> string
  val east : t
  val rotate: t -> int -> t
  val next_point: Point.t -> t -> Point.t
  val furthest : Point.t list -> t -> Point.t list
end

module type SQUARE = sig
  type t
  type elt
  val element_at : t -> Point.t -> elt 
  val sizeX : t -> int
  val sizeY : t -> int
  val inside: t -> Point.t -> bool
  val set   : t -> Point.t -> elt -> t
  val create : elt -> int -> int -> t
end

module Square : functor (_ : Util.Basic) -> SQUARE
