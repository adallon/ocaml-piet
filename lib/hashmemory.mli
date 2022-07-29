(**
   This module provides a data structure used to avoid costly recomputation.
   It also provides related function allowing to retrieve corners at no cost when possible.
 *)

(** Main type of the module. It can be seen as a memory containing information about previous computations *)
type t

(** Create a new memory for a program of n codels *)
val create : int -> t

(** Memorize that some codel block is associated with some group identifier *)
val add_group: t -> int -> Geometry.Point.t list -> int -> unit

(** Gets the value of the corner of some group of elements, in some direction and at some hand *)
val get_corner: t -> int -> Geometry.Direction.t -> Geometry.Hand.t -> int * Geometry.Point.t
