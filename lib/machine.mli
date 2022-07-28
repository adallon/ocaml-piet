(**
 * This module provides types and function for the main stack machine
 * It also contains the interpreter
 *)

type t

val interpreter: Program.t -> unit
