module CR : sig
  type t
  type elt = Codel.t
  val element_at : t -> Geometry.Point.t -> elt
  val set   : t -> Geometry.Point.t -> elt -> unit
end

module IOR : sig
  type t
  type elt = int option
  val element_at : t -> Geometry.Point.t -> elt
  val set   : t -> Geometry.Point.t -> elt -> unit
end

type t = 
  CR.t * IOR.t * int ref * Hashmemory.t

  (*
   *the codel array array represents the codel map
   *the int option array array represents the block numbers
   of the codels once it is set
   *the int represents the current max block number
   * the hashmemory is used to memorize block info to avoid costly
   * computings.
   * See hashmemory.ml
   *)

val inside : t -> Geometry.Point.t -> bool
val to_string : t -> string
val of_png: string -> t
val get_codel_block: t -> Geometry.Point.t -> Geometry.Point.t list
