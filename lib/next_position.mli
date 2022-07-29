(**
   This module provides the function that computes the next point 
   that should be reached by the explorer and other relevant information
  
 *)


(** This module provides a return type for the find function below.
    Its functions make this output readable.
 *)
module Result : sig
  
  (** Return type of find *)
  type t

  (** function telling whether a white codel has been seen or not *)
  val seen_white: t -> bool

  (** function giving the new value of DP after the movement *)
  val direction : t -> Geometry.Direction.t

  (** function giving the new value of CC after the movement *)
  val hand      : t -> Geometry.Hand.t

  (** function giving the size of the last color block seen.
      Returns 0 for white blocks
   *)
  val blocksize : t -> int

  (** function giving the point reached at the end of the movement.
   *)
  val point : t -> Geometry.Point.t
end

(** This module adds some memory to the program, so that costly recomputations are avoided. *)
module Memory : sig

  (** Main type of the Memory module. *)
  type t

  (** Creating an empty memory for a new program. *)
  val create : Program.t -> t

  (** Getting the program associated to the memory *)
  val get_prog : t -> Program.t
end

(** Function computing the next point to explore on a program augmented by the memory module.
    find m d h p computes the next point and other associated informations
    for a program represented by m, knowing that the DP value is d, the CC value is h,
    and the depart point is d.
 *)
val find: Memory.t -> Geometry.Direction.t -> Geometry.Hand.t -> Geometry.Point.t -> Result.t option
