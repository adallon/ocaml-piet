open Instructions

type t = Stack.t * Direction.direction * Direction.hand
(* Stack, Direction Pointer DP, Codel Chooser CC *)

let init_machine = Stack.empty,Direction.init_dir,Direction.init_hand

let get_direction ((_,b,_):t) = b
let get_hand      ((_,_,c):t) = c
let set d h ((a,_,_):t) = a,d,h

let next_state machine prev_blocksize =
  
  let (st,dp,cc) = machine in 
  let _ = Util.print_endline "next_state_executed" in
  (*
  let _ = print_string (Direction.direction_to_string dp ); print_string (Direction.hand_to_string cc); Util.print_endline "" in *)
  function
  | NoInst -> Util.print_endline "NoInst\n"; machine
  | Push -> Util.print_endline "Push";(Stack.push prev_blocksize st,dp,cc)
  | Pop  -> Util.print_endline "Pop";(Stack.pop st,dp,cc)
  | Add  -> Util.print_endline "Add";(Stack.add st,dp,cc)
  | Substract  -> Util.print_endline "Substract";(Stack.subst st,dp,cc)
  | Multiply   -> Util.print_endline "Multiply"; (Stack.mult  st,dp,cc)
  | Divide     -> Util.print_endline "Divide"; (Stack.div   st,dp,cc)
  | Mod        -> Util.print_endline "Mod"; (Stack.modulo st,dp,cc)
  | Not        -> Util.print_endline "Not"; (Stack.not st,dp,cc)
  | Greater    -> Util.print_endline "Greater"; (Stack.greater st,dp,cc)
  | Pointer    -> Util.print_endline "Pointer"; 
      let n0,st1 = Stack.get st in
      begin match n0 with
      | None    -> (st,dp,cc)
      | Some(n) -> (st1,Direction.rotate dp n,cc)
      end
  | Switch     -> Util.print_endline "Switch"; 
      let n0,st1 = Stack.get st in
      begin match n0 with
      | None    -> (st,dp,cc)
      | Some(n) -> 
          (st1,dp, if n mod 2 = 0 then cc else Direction.hand_switch cc)
      end
  | Duplicate  -> Util.print_endline "Duplicate"; (Stack.duplicate st,dp,cc)
  | Roll       -> Util.print_endline "Roll" ; (Stack.roll st,dp,cc)
  | InInt      -> Util.print_endline "InInt"; (Stack.inint st,dp,cc)
  | InChar     -> Util.print_endline "InChar"; (Stack.inchar st,dp,cc)
  | OutInt     -> Util.print_endline "OutIn"; (Stack.outint st,dp,cc)
  | OutChar    -> Util.print_endline "OutChar"; (Stack.outchar st,dp,cc)

