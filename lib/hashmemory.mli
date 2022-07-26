type t
val create : int -> t
val add_group: t -> int -> Geometry.Point.t list -> int -> unit
val get_corner: t -> int -> Geometry.Direction.t -> Geometry.Hand.t -> int * Geometry.Point.t
