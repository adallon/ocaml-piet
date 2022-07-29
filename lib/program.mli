(** 
   This module provides the description of the program itself.
   It also provides the basic functions allowing to reason about it.
   *)

(**
   Type of the programs
 *)
type t 

(** Function telling whether a point is inside a given program *)
val inside  : t -> Geometry.Point.t -> bool

(** Function computing the codel at some point *)
val codel_at: t -> Geometry.Point.t -> Codel.t

(** Function computing the group number at some point if there is some.
    Group number is mutable and can evolve during the execution.*)
val group_at: t -> Geometry.Point.t -> int option

(** Function returning the width in codels of the program *)
val sizeX : t -> int

(** Function returning the height in codels of the program *)
val sizeY : t -> int

(** Function changing the value of the group at some point. 
    Should only be called on points where the group is not set.*)
val set_group : t -> Geometry.Point.t -> int -> unit

(** Function representing programs as strings *)
val to_string : t -> string

(** Function computing the program representation of a png image *)
val of_png: string -> t

(** Function computing the color block 
    associated to a point in a program *)
val get_codel_block : t -> Geometry.Point.t -> Geometry.Point.t list

