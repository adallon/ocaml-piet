(*
type coord = int * int
type state = Codel_map.codel_map * coord * Machine.t
*)

let init_state map = (map,(0,0),Machine.init_machine)

let explorator state =
  let map,(cX,cY),machine = state in
  let dir  = Machine.get_direction machine in
  let hand = Machine.get_hand machine in
  match Codel_map.next_cases map dir hand (cX,cY) with
  | None -> None
  | Some(d,h,bs,x,y) -> 
      let new_state = (map,(x,y),Machine.set d h machine)
      in Some(new_state,bs)

let interpreter map =
  let () = Util.print_endline "interpreter" in
  let rec aux state =
    let _ = Util.print_endline "interpreter: aux" in
    let (_,coord0,_) = state in 
    let trans_opt = explorator state in
    match trans_opt with
    | None -> print_string "\nEnd of execution\n"
    | Some(state1,blocksize) ->
        let (_,coord1,mach1) = state1 in
        let th,tl = 
          Codel_map.codel_transition map coord0 coord1 
        in let inst  = Instructions.transition th tl
        in let mach2 = Machine.next_state mach1 blocksize inst
        in aux (map,coord1,mach2)
  in let state0 = init_state map 
  in aux state0

