module type Basic = sig
  type t
  (*
  val to_string : t -> string
  val to_int    : t -> int
  val diff : t -> t -> int
  *)
end

module Lightness: Basic

module Hue: Basic

type t = White | Black | Codel of Hue.t * Lightness.t

val to_string : t -> string
val diff : t -> t -> int* int
val of_rgb: int -> int -> int -> t
