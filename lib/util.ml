let coord_list_to_string l =
  let coord_to_string (i,j) = 
    String.concat "" ["(";string_of_int i;",";string_of_int j;")"]
  in String.concat ";" (List.map coord_to_string l)

let verb_level = 1

let print_string  verb s  = 
  if verb <= verb_level
  then print_string s  
  else ()

let print_int     verb s  = 
  if verb <= verb_level
  then print_int    s  
  else ()

let print_endline verb s  = 
  if verb <= verb_level 
  then print_endline s  
  else ()

let print_newline verb () = 
  if verb <= verb_level
  then print_newline () 
  else ()

let step_by_step = true
