type t = 
  (Codel.t array array) *
  (int option array array) * 
  int ref * Hashmemory.t

  (*
   *the codel array array represents the codel map
   *the int option array array represents the block numbers
   of the codels once it is set
   *the int represents the current max block number
   * the hashmemory is used to memorize block info to avoid costly
   * computings.
   * See hashmemory.ml
   *)

val codel_black_white: t -> int -> int -> bool * bool
val codel_map_to_string : t -> string
val png_to_map: string -> t
val get_codel_block: t -> Geometry.Point.t -> Geometry.Point.t list
