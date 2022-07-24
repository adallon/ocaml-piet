open Instructions

type t = Stack.t * Direction.direction * Direction.hand
(* Stack, Direction Pointer DP, Codel Chooser CC *)

let init_machine = Stack.empty,Direction.init_dir,Direction.init_hand

let get_direction ((_,b,_):t) = b
let get_hand      ((_,_,c):t) = c
let set d h ((a,_,_):t) = a,d,h

let next_state machine prev_blocksize inst =
  
  let (st,dp,cc) = machine in 
  let print_DPCC dp cc =
    begin
    Util.print_string 0 (Direction.direction_to_string dp ); 
    Util.print_string 0 ","; 
    Util.print_string 0 (Direction.hand_to_string cc); 
    Util.print_endline 0 "" 
    end
  in let _ = print_DPCC dp cc
    (*
     * Previous printing is the transition of the exploration machine.
     * See codel_transition in codel_map.ml
     *)
  in let aux =
  function
  | NoInst -> Util.print_endline 0 "NoInst"; machine
  | Push -> 
      Util.print_string 0 "Push ";
      Util.print_int 0 prev_blocksize ;
      Util.print_newline 0 () ;
      (Stack.push prev_blocksize st,dp,cc)
  | Pop  -> Util.print_endline 0 "Pop";(Stack.pop st,dp,cc)
  | Add  -> Util.print_endline 0 "Add";(Stack.add st,dp,cc)
  | Substract  -> Util.print_endline 0 "Substract";(Stack.subst st,dp,cc)
  | Multiply   -> Util.print_endline 0 "Multiply"; (Stack.mult  st,dp,cc)
  | Divide     -> Util.print_endline 0 "Divide"; (Stack.div   st,dp,cc)
  | Mod        -> Util.print_endline 0 "Mod"; (Stack.modulo st,dp,cc)
  | Not        -> Util.print_endline 0 "Not"; (Stack.not st,dp,cc)
  | Greater    -> Util.print_endline 0 "Greater"; (Stack.greater st,dp,cc)
  | Pointer    -> 
      let n0,st1 = Stack.get st in
      begin match n0 with
      | None    -> 
          let _ = 
            Util.print_string 0 "Pointer -> " ;
            print_DPCC dp cc;
          in (st,dp,cc)
      | Some(n) -> 
          let ndp = Direction.rotate dp n in
          let _ = 
            Util.print_string 0 "Pointer -> " ;
            print_DPCC ndp cc;
          in (st1,ndp,cc)
      end
  | Switch     -> 
      let n0,st1 = Stack.get st in
      begin match n0 with
      | None    -> 
          let _ = 
            Util.print_string 0 "Switch -> " ;
            print_DPCC dp cc;
          in (st,dp,cc)
      | Some(n) -> 
          let ncc = 
            if n mod 2 = 0 
            then cc 
            else Direction.hand_switch cc
          in
          let _ = 
            Util.print_string 0 "Switch -> " ;
            print_DPCC dp ncc;
          in
          (st1,dp,ncc)
      end
  | Duplicate  -> Util.print_endline 0 "Duplicate"; (Stack.duplicate st,dp,cc)
  | Roll       -> Util.print_endline 0 "Roll" ; (Stack.roll st,dp,cc)
  | InInt      -> Util.print_endline 0 "InInt"; (Stack.inint st,dp,cc)
  | InChar     -> Util.print_endline 0 "InChar"; (Stack.inchar st,dp,cc)
  | OutInt     -> Util.print_endline 0 "OutIn"; (Stack.outint st,dp,cc)
  | OutChar    -> Util.print_endline 0 "OutChar"; (Stack.outchar st,dp,cc)
  
  in let (nst,ndp,ncc) = aux inst
  in let _ = 
    Util.print_endline 0 (Stack.to_string nst)
  in (nst,ndp,ncc)

