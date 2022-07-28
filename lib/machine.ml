open Instructions
open Geometry

(*
 * The Piet stack machine is a stack and the DP and CC,
 * plus the current position in the map which is handled elsewhere.
 * We first define the stack submodule.
 * The complete machine is below.
 *)

(**
 * This module provides types and function for the main stack machine
 * It also contains the interpreter
 *)


module type Stack_type = sig
  type t
  val empty:t
  val push: int -> t -> t 
  val pop     : t -> t
  val add     : t -> t
  val subst   : t -> t
  val mult    : t -> t
  val div     : t -> t
  val modulo  : t -> t
  val not     : t -> t
  val greater : t -> t
  val get     : t -> int option * t 
  val duplicate : t -> t
  val roll    : t -> t
  val inint   : t -> t 
  val inchar  : t -> t 
  val outint  : t -> t 
  val outchar : t -> t 
  val to_string : t -> string
end 

module Stack : Stack_type =
  struct
  type t = int list

  let to_string x = 
    let main = String.concat ";" (List.map string_of_int x)
    in String.concat "" ["[";main;"]"]

  let empty = []

  (*
  let test1 = [0;1;2;5;7]
  let test2 = [5;-3;2;12]
  *)

  let push n (x:t) = n::x

  (*
  let () =
    assert(push 6 empty = [6]);
    assert(push 7 test1 = [7;0;1;2;5;7])
  *)

  let pop (x:t) =
    match x with
    | [] -> []
    | _::t -> t

  (*
  let () =
    assert(pop empty = []);
    assert(pop test1 = [1;2;5;7]);
    assert(pop test2 = [-3;2;12])
  *)

  let add (x:t) =
    match x with
    | a::(b::t) -> (a+b)::t
    | l -> l

  (*
  let () =
    assert(add empty = []);
    assert(add [3] = [3]);
    assert(add test1 = [1;2;5;7]);
    assert(add test2 = [2;2;12])
  *)


  let subst (x:t) =  
    match x with
    | a::(b::t) -> (b-a)::t
    | l -> l

  (*
  let () =
    assert(subst empty = []);
    assert(subst [3] = [3]);
    assert(subst test1 = [1;2;5;7]);
    assert(subst test2 = [-8;2;12])
  *)

  let mult (x:t) =  
    match x with
    | a::(b::t) -> (b*a)::t
    | l -> l

  (*
  let () =
    assert(mult empty = []);
    assert(mult [3] = [3]);
    assert(mult test1 = [0;2;5;7]);
    assert(mult test2 = [-15;2;12])
  *)

  let div (x:t) =  
    match x with
    | 0::t -> 0::t
    | a::(b::t) -> (b/a)::t
    | l -> l

  (*
  let () =
    assert(div empty = []);
    assert(div [3] = [3]);
    assert(div test1 = [0;1;2;5;7]);
    assert(div test2 = [0;2;12])
  *)

  let modulo (x:t) =  
    match x with
    | 0::t -> 0::t
    | a::(b::t) ->
      let x = if a>0 
      then  (a+(b mod a))   mod a
      else  ((b mod a) - a) mod a
      in x::t
    | l -> l

  (*
  let () =
    assert(modulo empty = []);
    assert(modulo [3] = [3]);
    assert(modulo test1 = [0;1;2;5;7]);
    assert(modulo test2 = [2;2;12])
  *)

  let not = function
    | [] -> []
    | a::t -> if a = 0 then 1::t else 0::t

  (*
  let () =
    assert(not empty = []);
    assert(not [3] = [0]);
    assert(not test1 = [1;1;2;5;7]);
    assert(not test2 = [0;-3;2;12])
  *)

  let greater = function
    | a::(b::t) -> if b > a then 1::t else 0::t
    | l -> l

  (*
  let () =
    assert(greater empty = []);
    assert(greater [3] = [3]);
    assert(greater test1 = [1;2;5;7]);
    assert(greater test2 = [0;2;12])
  *)

  let get (x:t) =
    (* pointer and switch will use this function *)
   match x with
   | n::t -> Some(n),t
   | [] -> None,[]

  (*
  let () =
    assert(get empty = (None,[]));
    assert(get [3]   = (Some(3),[]));
    assert(get test1 = (Some(0),pop test1));
    assert(get test2 = (Some(5),pop test2))
  *)

  let duplicate = function
    | a::t -> a::(a::t)
    | [] -> []

  (*
  let () =
    assert(duplicate empty = []);
    assert(duplicate [3]   = [3;3]);
    assert(duplicate test1 = push 0 test1);
    assert(duplicate test2 = push 5 test2)
  *)

  let roll_aux nrolls depth x =
    let rec get_firsts n res = function
      | l when n = 0 -> Some(res,l)
      | [] -> None
      | a::t -> get_firsts (n-1) (a::res) t
    in let nrolls = (depth + (nrolls mod depth)) mod depth
      (* 2 + (1 mod 2) mod 2 *)
    in let firsts = get_firsts nrolls [] x
    in begin match firsts with
    | None -> x
    | Some(r,l) ->
        let seconds = get_firsts (depth - nrolls) [] l in
          match seconds with
          | None -> x
          | Some(r1,l1) -> 
              List.rev_append r1 (List.rev_append r l1)
        end
  
  let roll = function
    | a::(b::t) -> if b > 0 then roll_aux a b t else a::(b::t)
    | t -> t
  
  (*
  let () =
    (* Failure cases *)
      (* Not enough values *)
    assert(roll empty = []);
    assert(roll [3]   = [3]);
    assert(roll test2 = test2);
    (* No move cases *)
      (* Roll = 0 *)
    assert(roll test1 = [2;5;7]);
      (* Depth = 1 *)
    assert(roll (not test1) = [2;5;7]);
    (* Move cases *)
      (* Roll:1 Depth:2 *)
    assert(roll (add test1) = [7;5]);
      (* roll:2 depth:3 *)
    assert(roll (push 2 (push 3 test2)) = [2;5;-3;12])
  *)
  
  let inint x =
    let () = print_string "?" in
    match read_int_opt () with
    | None -> x
    | Some(n) -> n::x
  
  let inchar x =
    let s = read_line () in 
    if String.length s = 1
    then (Char.code s.[0])::x
    else x
  
  let outint = function
    | x::t -> print_int x ; t
    | [] -> []
  
  let outchar = function
    | x::t -> 
        if 0 <= x && x < 256 then 
          let _ = 
            print_char (Char.chr x) ; 
            Util.print_newline 0 () 
          in t
        else x::t
    | [] -> []
end  

module Stack_machine :
 sig
   type t 
   val initial_state : t
   val get_direction : t -> Direction.t
   val get_hand      : t -> Hand.t
   val set : Direction.t -> Hand.t -> t -> t
   val next_state : t -> int -> Instructions.t -> t

 end
  = struct
  type t = Stack.t * Direction.t * Hand.t
  (* Stack, Direction Pointer DP, Codel Chooser CC *)

  let initial_state = Stack.empty,Direction.east,Hand.Left
  let get_direction ((_,b,_):t) = b
  let get_hand      ((_,_,c):t) = c
  let set d h ((a,_,_):t) = a,d,h

  let next_state machine prev_blocksize inst =
    let (st,dp,cc) = machine in 
    let print_DPCC dp cc =
      begin
      Util.print_string 0 (Direction.to_string dp ); 
      Util.print_string 0 ","; 
      Util.print_string 0 (Hand.to_string cc); 
      Util.print_endline 0 "" 
      end
    in let _ = print_DPCC dp cc
      (*
       * Previous printing is the transition of the exploration machine.
       * See codel_transition in codel_map.ml
      *)
    in let aux = function
      | NoInst -> 
          Util.print_endline 0 "NoInst"; machine
      | Push -> 
        Util.print_string 0 "Push ";
        Util.print_int 0 prev_blocksize ;
        Util.print_newline 0 () ;
        (Stack.push prev_blocksize st,dp,cc)
      | Pop  -> Util.print_endline 0 "Pop";(Stack.pop st,dp,cc)
      | Add  -> Util.print_endline 0 "Add";(Stack.add st,dp,cc)
      | Substract  -> 
          Util.print_endline 0 "Substract";(Stack.subst st,dp,cc)
      | Multiply   -> 
          Util.print_endline 0 "Multiply"; (Stack.mult  st,dp,cc)
      | Divide     -> 
          Util.print_endline 0 "Divide"; (Stack.div   st,dp,cc)
      | Mod        -> 
          Util.print_endline 0 "Mod"; (Stack.modulo st,dp,cc)
      | Not        -> 
          Util.print_endline 0 "Not"; (Stack.not st,dp,cc)
      | Greater    -> 
          Util.print_endline 0 "Greater"; (Stack.greater st,dp,cc)
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
              else Hand.switch cc
            in let _ = 
              Util.print_string 0 "Switch -> " ;
              print_DPCC dp ncc;
            in (st1,dp,ncc)
        end
      | Duplicate  -> 
        Util.print_endline 0 "Duplicate"; (Stack.duplicate st,dp,cc)
      | Roll       -> 
        Util.print_endline 0 "Roll" ; (Stack.roll st,dp,cc)
      | InInt      -> 
        Util.print_endline 0 "InInt"; (Stack.inint st,dp,cc)
      | InChar     -> 
        Util.print_endline 0 "InChar"; (Stack.inchar st,dp,cc)
      | OutInt     -> 
        Util.print_endline 0 "OutIn"; (Stack.outint st,dp,cc)
      | OutChar    -> 
        Util.print_endline 0 "OutChar"; (Stack.outchar st,dp,cc)
  
  in let (nst,ndp,ncc) = aux inst
  in let _ = Util.print_endline 0 (Stack.to_string nst)
  in (nst,ndp,ncc)
end

type t = Next_position.Memory.t * Point.t * Stack_machine.t 

let initial_state prog = 
  ((Next_position.Memory.create prog),Point.to_point (0,0),Stack_machine.initial_state)

let explorator (state:t) =
  let prog,cur_p,stack = state in
  let dir  = Stack_machine.get_direction stack in
  let hand = Stack_machine.get_hand stack in
  match Next_position.find prog dir hand cur_p with
  | None -> None
  | Some(next_pos) -> 
      let wh   = Next_position.Result.seen_white next_pos in
      let dir  = Next_position.Result.direction  next_pos in
      let hand = Next_position.Result.hand       next_pos in
      let bs   = Next_position.Result.blocksize  next_pos in
      let p    = Next_position.Result.point      next_pos in
      let new_state = (prog,p,Stack_machine.set dir hand stack)
      in let _ = 
        if Util.get_step_by_step ()
        then let _ = print_string ">"; read_line () in ()
        else () 
      in Some((new_state:t),bs,wh)

let step state =
  let (mem,p0,_) = state in
  let trans_opt = explorator state in 
  match trans_opt with
  | None -> None
  | Some(state1,blocksize,wh) ->
    let (_,p1,(stack1:Stack_machine.t)) = (state1:t) in
    let prog = Next_position.Memory.get_prog mem in
    let c0 = Program.codel_at prog p0 in
    let c1 = Program.codel_at prog p1 in
    let c0_string = Codel.to_string c0 in
    let c1_string = Codel.to_string c1 in
    let _ =
      Util.print_newline 0 ();
      Util.print_string 0 (Point.to_string p0);
      Util.print_string 0 ":";
      Util.print_string 0 c0_string;
      Util.print_string 0 " -> ";
      Util.print_string 0 (Point.to_string p1);
      Util.print_string 0 ":";
      Util.print_string 0 c1_string;
      Util.print_string 0 " ";
    in let stack2 = 
      Stack_machine.next_state stack1 blocksize (Instructions.transition wh c0 c1)
    in Some((mem,p1,stack2))

let interpreter prog =
  let rec aux state =
    match step state with
    | None -> Util.print_string 0 "\nEnd of execution\n"
    | Some(state) -> aux state
  in let state = initial_state prog
  in aux state
