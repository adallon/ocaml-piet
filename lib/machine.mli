type t

val initial_state: t

val next_state : t -> int -> Instructions.t -> t
(* machine, blocksize, instruction *)

val get_direction: t -> Geometry.Direction.t
val get_hand     : t -> Geometry.Hand.t
val set          : Geometry.Direction.t -> Geometry.Hand.t -> t -> t

