(**
   This module implements the main stack machine.
   It mainly contains the interpreter.
 *)

(** This function provides the main functionality of ocaml_piet.
    It takes a Program.t as an input. 
    It should have been computed from the image before. 
    *)
val interpreter: Program.t -> unit
