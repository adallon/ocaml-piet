type direction
type hand

val init_dir: direction
val init_hand: hand

val hand_switch: hand -> hand
val rotate: direction -> int -> direction
val furthest : (int * int) list -> direction -> (int*int) list

val rotate_hand: direction -> hand -> direction
val next_point: int*int -> direction -> int*int
val dir_hand_order: direction -> hand -> (direction*hand) list


