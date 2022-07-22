open Ocaml_piet

(*
let path = "/home/antoine/dépôts/perso/ocaml-piet/examples/alpha_filled.png"
*)

let path = Sys.argv.(1);;
print_endline path 
let map = Codel_map.png_to_map path;;
    (* Ocaml_piet.Codel_map.codel_map_example  *)
(* print_string (Codel_map.codel_map_to_string map);*)
print_newline ();
Explorator.interpreter map;;
