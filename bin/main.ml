
let _ =
  let map = Ocaml_piet.Codel_map.codel_map_example in
  print_string (Ocaml_piet.Codel_map.codel_map_to_string map);
  print_newline ();
  print_string (Ocaml_piet.Util.coord_list_to_string (Ocaml_piet.Codel_map.get_color_block map 2 2));
  print_newline ();
;;
