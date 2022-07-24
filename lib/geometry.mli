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

module type MAP = sig
  type t
  type elt
  val element_at : t -> int -> int -> elt 
  val sizeX : t -> int
  val sizeY : t -> int
end

module Map :
  functor (_ : Codel.Basic) -> MAP
