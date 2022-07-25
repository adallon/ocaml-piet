let verb_level = ref 0
let step_by_step = ref false

let set_verb_level v = verb_level :=  v
let set_steps b = step_by_step :=  b

let get_step_by_step () = !step_by_step


let print_string  verb s  = 
  if verb < !verb_level
  then print_string s  
  else ()

let print_int     verb s  = 
  if verb < !verb_level
  then print_int    s  
  else ()

let print_endline verb s  = 
  if verb < !verb_level 
  then print_endline s  
  else ()

let print_newline verb () = 
  if verb < !verb_level
  then print_newline () 
  else ()

module type Basic = sig
  type t
  (*
  val to_string : t -> string
  val to_int    : t -> int
  val diff : t -> t -> int
  *)
end


