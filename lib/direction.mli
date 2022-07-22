type direction
type hand

val direction_to_string : direction -> string 
val hand_to_string : hand -> string

val init_dir: direction
val init_hand: hand

val hand_switch: hand -> hand
val rotate: direction -> int -> direction
val furthest : (int * int) list -> direction -> (int*int) list

val rotate_hand: direction -> hand -> direction
val next_point: int*int -> direction -> int*int
val dir_hand_order: direction -> hand -> (direction*hand) list


