module CR : sig
  type t
  type elt = Codel.t
  val element_at : t -> Geometry.Point.t -> elt
  val sizeX : t -> int
  val sizeY : t -> int
  val inside: t -> Geometry.Point.t -> bool
  val set   : t -> Geometry.Point.t -> elt -> unit
  val create : elt -> int -> int -> t
  val iter : (Geometry.Point.t -> elt -> unit) -> t -> unit
end

(*
val elt_of_codel: Codel.t -> CR.t
val elt_to_codel: CR.t -> Codel.t
*)

module IOR : sig
  type t
  type elt = int option
  val element_at : t -> Geometry.Point.t -> elt
  val sizeX : t -> int
  val sizeY : t -> int
  val inside: t -> Geometry.Point.t -> bool
  val set   : t -> Geometry.Point.t -> elt -> unit
  val create : elt -> int -> int -> t
  val iter : (Geometry.Point.t -> elt -> unit) -> t -> unit
end

(*
val elt_of_intopt: int option -> IOR.elt
val elt_to_intopt: IOR.elt  -> int option
*)

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

val black_white: t -> Geometry.Point.t -> bool * bool
val to_string : t -> string
val of_png: string -> t
val get_codel_block: t -> Geometry.Point.t -> Geometry.Point.t list
