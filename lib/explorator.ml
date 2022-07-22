(*
type coord = int * int
type state = Codel_map.codel_map * coord * Machine.t
*)

let init_state map = (map,(0,0),Machine.init_machine)

let explorator state =
  let map,(cX,cY),machine = state in
  let dir  = Machine.get_direction machine in
  let hand = Machine.get_hand machine in
  let color_block = Codel_map.get_codel_block map cX cY in
  let next_cases =
    let dir_and_hands = Direction.dir_hand_order dir hand in
    let to_map (d,h) =
      let f_list = Direction.furthest color_block d in
      let cc_dir = Direction.rotate_hand d h in
      match Direction.furthest f_list cc_dir with
      | [x1,y1] -> (d,h, Direction.next_point (x1,y1) d)
      | _ -> assert(false) (* only one furthest in each direction *)
    in List.map to_map dir_and_hands
  in let rec get_next_coord = function
    | [] -> None
    | (d,h,(x,y))::t ->
        let rec aux_get_next (d,h,(x,y)) =
          let not_black,white = Codel_map.codel_black_white map x y in
          if white
          then aux_get_next (d,h,Direction.next_point (x,y) d)
          else if not_black
          then Some(d,h,(x,y))
          else get_next_coord t
        in aux_get_next (d,h,(x,y))
  in match get_next_coord next_cases with
  | None -> None
  | Some(d,h,(x,y)) -> 
      let new_state = (map,(x,y),Machine.set d h machine)
      in Some(new_state,List.length color_block)

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

