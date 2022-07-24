module Point : sig
  type t
  val to_point : int * int -> t
  val x : t -> int
  val y : t -> int
  val is_close: t -> t list -> bool
  val to_string: t -> string
end

type direction
type hand = Left | Right

val direction_to_string : direction -> string 
val hand_to_string : hand -> string

val init_dir: direction
val init_hand: hand

val hand_switch: hand -> hand
val rotate: direction -> int -> direction
val furthest : Point.t list -> direction -> Point.t list

val next_point: Point.t -> direction -> Point.t
val dir_hand_order: direction -> hand -> (direction*hand) list


