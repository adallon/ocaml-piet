(** This module provides the description of the codel type 
 * and the basic function for it. Lightness and hue are included in the type
 * but are hidden by the interface.*)

(** This module describes the lightness levels *)
module Lightness =
  struct

    (** Basic lightness levels *) 
    type t = Light | Normal | Dark

    (** Lightness as string *) 
    let to_string = function
    | Light  -> "l"
    | Normal -> "n"
    | Dark   -> "d"

    (** Lightness as int. It helps computing the distance 
     * between two lightness levels *) 
    let to_int = function
    | Light  -> 0
    | Normal -> 1
    | Dark   -> 2

    (** Distance between two lightness levels.
     * Must be a nonnegative number *) 
    let diff a b = (to_int b - to_int a + 3) mod 3
  end

(** Describes the different hues *)
module Hue = 
  struct

    (** Basic hues *)
    type t = Red | Yellow | Green | Cyan | Blue | Magenta

    (** Hue as string *) 
    let to_string = function
      | Red     -> "R"
      | Yellow  -> "Y"
      | Green   -> "G"
      | Cyan    -> "C"
      | Blue    -> "B"
      | Magenta -> "M" 

    (** Hue as int. Helps to compute the distance. *) 
    let to_int = function
      | Red     -> 0
      | Yellow  -> 1
      | Green   -> 2
      | Cyan    -> 3
      | Blue    -> 4
      | Magenta -> 5

    (** Distance between two hues.
     * Must be a nonnegative number *) 
    let diff a b = (to_int b - to_int a + 3) mod 3
  end


(** Codel type built from hue and lightness *)
type t = White | Black | Codel of Hue.t * Lightness.t

(** The black element is the only element visible from outside *)
let black = Black

(** Converter of codels to strings. *)
let to_string = function
  | White -> " W"
  | Black -> " N"  (* as Nothing or Night to avoid confusion with Blue *)
  | Codel (h,l) -> 
      String.concat "" [Lightness.to_string l ; Hue.to_string h]

(** Computing the difference between two codels.
 * Should not be called on non-colored codels. *) 
let diff c1 c2 =
  match c1,c2 with
  | White,_|_,White|Black,_|_,Black -> assert(false)
  | Codel(h0,l0),Codel(h1,l1) -> (Hue.diff h0 h1, Lightness.diff l0 l1)

(** Gets codels from rgb representations of bits.
 * When the color does not correspond to any standard hue and lightness,
 * it is interpreted as white 
 *
 * @param r red level of a pixel
 * @param g green level of a pixel
 * @param b blue level of a pixel
 *)
let of_rgb r g b =
  match r,g,b with
  | 255,255,255 -> White
  |   0,0,0     -> Black

  | 255,192,192 -> Codel(Red,Light)
  | 255,  0,  0 -> Codel(Red,Normal)
  | 192,  0,  0 -> Codel(Red,Dark)

  | 255,255,192 -> Codel(Yellow,Light)
  | 255,255,  0 -> Codel(Yellow,Normal)
  | 192,192,  0 -> Codel(Yellow,Dark)

  | 192,255,192 -> Codel(Green,Light)
  |   0,255,  0 -> Codel(Green,Normal)
  |   0,192,  0 -> Codel(Green,Dark)

  | 192,255,255 -> Codel(Cyan ,Light)
  |   0,255,255 -> Codel(Cyan ,Normal)
  |   0,192,192 -> Codel(Cyan ,Dark)

  | 192,192,255 -> Codel(Blue ,Light)
  |   0,  0,255 -> Codel(Blue ,Normal)
  |   0,  0,192 -> Codel(Blue ,Dark)

  | 255,192,255 -> Codel(Magenta, Light)
  | 255,  0,255 -> Codel(Magenta, Normal)
  | 192,  0,192 -> Codel(Magenta, Dark)

  |   _,  _,  _ -> White


(**
 * Function telling whether a codel is white
 *)
let is_white c = (c = White) 

(**
 * Function telling whether a codel is black
 *)
let is_black c = (c = Black)
