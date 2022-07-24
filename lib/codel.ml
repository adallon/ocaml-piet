module type Basic = sig
  type t
  (*
  val to_string : t -> string
  val to_int    : t -> int
  val diff : t -> t -> int
  *)
end

module Lightness =
  struct
    type t = Light | Normal | Dark

    let to_string = function
    | Light  -> "l"
    | Normal -> "n"
    | Dark   -> "d"

    let to_int = function
    | Light  -> 0
    | Normal -> 1
    | Dark   -> 2

    let diff a b = (to_int b - to_int a + 3) mod 3
  end

module Hue = 
  struct
    type t = Red | Yellow | Green | Cyan | Blue | Magenta
    let to_int = function
      | Red     -> 0
      | Yellow  -> 1
      | Green   -> 2
      | Cyan    -> 3
      | Blue    -> 4
      | Magenta -> 5

    let to_string = function
      | Red     -> "R"
      | Yellow  -> "Y"
      | Green   -> "G"
      | Cyan    -> "C"
      | Blue    -> "B"
      | Magenta -> "M" 

    let diff a b = (to_int b - to_int a + 6) mod 6
  end

type t = White | Black | Codel of Hue.t * Lightness.t

let to_string = function
  | White -> " W"
  | Black -> " N" (* as Noir or Night *)
  | Codel (h,l) -> 
      String.concat "" [Lightness.to_string l ; Hue.to_string h]
 
let diff c1 c2 =
  match c1,c2 with
  | White,_|_,White|Black,_|_,Black -> failwith("Codel.diff should not be used on black or white codels")
  | Codel(h0,l0),Codel(h1,l1) -> Hue.diff h0 h1, Lightness.diff l0 l1

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


