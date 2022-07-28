(**
 * Module describing utilitary functions
 *
 *)


(**
 * Function allowing to set the parameter value steps
 * If set at true, it corresponds to the interactive mode
 *)
val set_steps : bool -> unit

(**
 * Function allowing to set the parameter value steps
 * If set at true, it corresponds to the interactive mode
 *)
val get_step_by_step : unit -> bool

(**
 * Function allowing to set the verbosity level
 *)
val set_verb_level : int -> unit

(**
 * Function printing strings depending on verbosity level
 *)
val print_string: int -> string -> unit

(**
 * Function printing ints depending on verbosity level
 *)
val print_int: int -> int -> unit

(**
 * Function like Stdlib.print_endline, but with verbosity level
 *)
val print_endline: int -> string -> unit

(**
 * Function like Stdlib.print_newline, but with verbosity level
 *)
val print_newline: int -> unit -> unit


(**
 * Module with just a type
 *)
module type Basic = sig
  type t
end


