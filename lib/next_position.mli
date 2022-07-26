module Result : sig
  type t
  val seen_white: t -> bool
  val direction : t -> Geometry.Direction.t
  val hand      : t -> Geometry.Hand.t
  val blocksize : t -> int
  val point : t -> Geometry.Point.t
end

module Memory : sig
  type t
  val create : Program.t -> t
  val get_prog : t -> Program.t
end

val find: Memory.t -> Geometry.Direction.t -> Geometry.Hand.t -> Geometry.Point.t -> Result.t option
