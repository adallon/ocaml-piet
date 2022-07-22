type codel_map

val codel_black_white: codel_map -> int -> int -> bool * bool

val codel_map_to_string: codel_map -> string

val codel_transition : codel_map -> int*int -> int*int -> int*int
val codel_map_size : codel_map -> int*int

val codel_map_example: codel_map

val get_codel_block: codel_map -> int -> int -> (int*int) list

