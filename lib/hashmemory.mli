(**
   This module provides a data structure used to avoid costly recomputation
   It also provides related function allowing to retrieve corners at no cost when possible
 *)


type t
val create : int -> t
val add_group: t -> int -> Geometry.Point.t list -> int -> unit
val get_corner: t -> int -> Geometry.Direction.t -> Geometry.Hand.t -> int * Geometry.Point.t
