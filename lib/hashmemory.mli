type t
val create : int -> t
val add_group: t -> int -> Geometry.coord list -> int -> unit
val get_corner: t -> int -> Geometry.direction -> Geometry.hand -> int * Geometry.coord 
