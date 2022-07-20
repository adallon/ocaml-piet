type lightness = Light | Normal | Dark

let lightness_val = function
  | Light  -> 0
  | Normal -> 1
  | Dark   -> 2

let lightness_diff a b = 
  (lightness_val b - lightness_val a + 3) mod 3


type hue = Red | Yellow | Green | Cyan | Blue | Magenta

let hue_val = function
  | Red     -> 0
  | Yellow  -> 1
  | Green   -> 2
  | Cyan    -> 3
  | Blue    -> 4
  | Magenta -> 5

let hue_diff a b = 
  (hue_val b - hue_val a + 6) mod 6

type codel = White | Black | Codel of hue * lightness
