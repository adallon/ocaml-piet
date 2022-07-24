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

type direction

val direction_to_string : direction -> string 

val init_dir: direction
val rotate: direction -> int -> direction
val furthest : Point.t list -> direction -> Point.t list

val next_point: Point.t -> direction -> Point.t
val dir_hand_order: direction -> Hand.t -> (direction*Hand.t) list


