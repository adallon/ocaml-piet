(** 
 * This module provides the description of the program itself.
 * It also provides the basic functions allowing to reason about it.
 * *)

type t 

val inside  : t -> Geometry.Point.t -> bool
val codel_at: t -> Geometry.Point.t -> Codel.t
val group_at: t -> Geometry.Point.t -> int option
val sizeX : t -> int
val sizeY : t -> int
val set_group : t -> Geometry.Point.t -> int -> unit
val to_string : t -> string
val of_png: string -> t
val get_codel_block : t -> Geometry.Point.t -> Geometry.Point.t list

