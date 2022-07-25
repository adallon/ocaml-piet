module Lightness: Util.Basic

module Hue: Util.Basic

type t = White | Black | Codel of Hue.t * Lightness.t

val to_string : t -> string
val diff : t -> t -> int*int
val of_rgb: int -> int -> int -> t
val is_white : t -> bool
val is_black : t -> bool
