type t

val init_machine: t

val next_state : t -> int -> Instructions.instr -> t
(* machine, blocksize, instruction *)

val get_direction: t -> Direction.direction
val get_hand     : t -> Direction.hand
val set          : Direction.direction -> Direction.hand -> t -> t

