(** This module provides the description of the codel type 
   and the basic function for it. Lightness and hue are included in the type
   but are hidden by the interface.*)

(** Main type of the module. Describes individual codels,
   which are an abstraction of pixel of colors. *)
type t 

(** A representation of the black codel. 
   Only the black codel is visible from outside.
   It is used to initialize codel data structures *)
val black: t

(** A representation of codels as strings.*)
val to_string : t -> string

(** A computation of the difference between two codels 
   in hue and lightness *)
val diff : t -> t -> int*int

(** Gets codels from rgb representations of bits.
   When the color does not correspond to any standard hue and lightness,
   it is interpreted as white *)
val of_rgb: int -> int -> int -> t

(**
   Function telling whether a codel is white
 *)
val is_white : t -> bool

(**
   Function telling whether a codel is black
 *)
val is_black : t -> bool
