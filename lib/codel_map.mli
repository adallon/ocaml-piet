type codel_map

(*
val codel_black_white: codel_map -> int -> int -> bool * bool
val codel_transition : codel_map -> int*int -> int*int -> int*int
val next_cases: codel_map -> Direction.direction -> Direction.hand -> int*int -> (Direction.direction * Direction.hand * int * int * int) option
*)

val codel_map_to_string: codel_map -> string

val interpreter: codel_map -> unit
val png_to_map : string -> codel_map
