let coord_list_to_string l =
  let coord_to_string (i,j) = 
    String.concat "" ["(";string_of_int i;",";string_of_int j;")"]
  in String.concat ";" (List.map coord_to_string l)

let print_string s = (*print_string s *) ()
let print_endline s = (*print_endline s *) ()
let print_newline () = (*print_newline () *) ()
