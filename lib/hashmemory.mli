type t
val create : int -> t
val add_group: t -> int -> Util.coord list -> int -> unit
val get_corner: t -> int -> Direction.direction -> Direction.hand -> int * Util.coord 
