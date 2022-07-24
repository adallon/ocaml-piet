type coord
val coord_list_to_string : coord list -> string
val to_coord : int * int -> coord
val x : coord -> int
val y : coord -> int
val print_coord_trace: coord -> unit
val is_close: coord -> coord list -> bool

type direction
type hand = Left | Right

val direction_to_string : direction -> string 
val hand_to_string : hand -> string

val init_dir: direction
val init_hand: hand

val hand_switch: hand -> hand
val rotate: direction -> int -> direction
val furthest : coord list -> direction -> coord list

val next_point: coord -> direction -> coord
val dir_hand_order: direction -> hand -> (direction*hand) list


