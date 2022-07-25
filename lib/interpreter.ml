let interpreter map =
  let rec aux state =
    match Machine.step state with
    | None -> Util.print_string 0 "\nEnd of execution\n"
    | Some(state) -> aux (state)
  in let state = Machine.initial_state map 
  in aux state
