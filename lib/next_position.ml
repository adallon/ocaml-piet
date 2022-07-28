open Geometry

module Result = struct
  type t = bool * Direction.t  * Hand.t * int * Point.t

  let seen_white (wh, _ ,_,_,_ )   = wh
  let direction  ( _,dir,_,_,_)    = dir
  let hand       ( _, _ ,hand,_,_) = hand
  let blocksize  ( _, _ ,  _,bs,_) = bs
  let point (_ ,_,_,_,p) = p
end

module Memory = struct
  type t = Program.t * Hashmemory.t * (int ref)
  
  let get_new_g (_,_,maxG) = 
    let g = !maxG in let _ = maxG := !maxG+1 in g 
  
  let add_group (_,tab,_) g block size = 
    Hashmemory.add_group  tab g block size
  
  let get_corner (_,tab,_) g d h = 
    Hashmemory.get_corner tab g d h

  let create prog =
    let size = (Program.sizeX prog) * (Program.sizeY prog)
    in (prog,Hashmemory.create size, ref 0)
 
  let get_prog (prog,_,_) = prog 
end


let get_next_coord (mem:Memory.t) wh d p =
  (* prog: program
   * wh: seen white codel?
   * d : last direction?
   * p : point *)
  let prog = Memory.get_prog mem in
  let rec aux p0 wh p =
    let black,white =
      if Program.inside prog p
      then 
        let c = Program.codel_at prog p in
        Codel.is_black c, Codel.is_white c
      else true,false
      (* the outside is handled as black cases *)
    in if white
    (* the codel is white: we continue until we find a border,
     * a colored block or a black codel *)
    then aux p0 true (Direction.next_point p d)
    else if not(black)
    (* it is neither white nor black: it is a colored block *)
    then wh,Some(p)
    else (* it is black or a border *)
      if wh = false ||(p0 = p)
      (* We were never at a white block, 
       * or the direction is completely obstructed.
       *)
      then wh,None 
      else true,Some(p0)
  in aux p wh p 

let get_possibility mem cur_p (d,h) =
  let _ =
    Util.print_string  1 "  Current direction ";
    Util.print_endline 1 (Direction.to_string d); 
    Util.print_string  1 "  Current hand ";
    Util.print_endline 1 (Hand.to_string h); 
  in let prog = Memory.get_prog mem
  in let c,g_op = 
    (Program.codel_at prog cur_p), (Program.group_at prog cur_p)
  in match Codel.is_white c, g_op  with
  | true,Some(_) ->
    let _ =
      Util.print_endline 1 "  We are at a white codel";
      Util.print_endline 1 "    (Seen before)";
    in let wh,next_p = 
      get_next_coord mem true d (Direction.next_point cur_p d)
    in (wh,0,next_p)
  | true,None ->
    let _ =
      Util.print_endline 1 "  We are at a white codel";
      Util.print_endline 1 "    (Not seen yet)";
    in
    let gVal = Memory.get_new_g (mem:Memory.t) in
    let    _ = 
      Program.set_group (Memory.get_prog mem) cur_p gVal
    in
    let wh,next_p =
      get_next_coord mem true d (Direction.next_point cur_p d)
    (* in let _ = Hashtbl.add tab (gVal,d,h) (wh,0,next_p) *)
    in (wh,0,next_p)
    (* = Hashtbl.find tab (g,d,h) *)
  | false,Some(g) ->
    assert(not(Codel.is_black c));
    let _ =
        Util.print_endline 1 "  We are at a color codel";
        Util.print_endline 1 "    (Color block seen before)";
    in let size,corner = Memory.get_corner mem g d h
    in let wh,next_p = 
      get_next_coord mem false d (Direction.next_point corner d)
      in (wh,size,next_p)
  | false,None ->
    assert(not(Codel.is_black c));
    let _ =
      Util.print_endline 1 "  We are at a color codel";
      Util.print_endline 1 "    (Color block not seen yet)";
    in
    let color_block = Program.get_codel_block prog cur_p in
    let blocksize = List.length color_block in
    let gVal = Memory.get_new_g mem in
    let _ =
      let rec aux = function
        | [] -> ()
        | p::t ->
            begin Program.set_group (Memory.get_prog mem) p gVal; aux t end

      in aux color_block
    in 
    let _ = Memory.add_group mem gVal color_block blocksize in
    let size,corner = Memory.get_corner mem gVal d h in
    let _ = assert(size = blocksize) in
    let (wh,next_p) = 
      get_next_coord mem false d (Direction.next_point corner d)
    in (wh,size,next_p)


let rec find_direction mem cur_p = function
  | [] -> 
      let _ =
        Util.print_endline 1 "  No direction has been found. Execution will terminate."
      in None
  | (d,h)::t -> 
      let _ =
        Util.print_string 1 " Trying with direction ";
        Util.print_string 1 (Direction.to_string d); 
        Util.print_string 1 " and hand ";
        Util.print_endline 1 (Hand.to_string h); 
      in begin match get_possibility mem cur_p (d,h) with
      | (wh,bs,Some(p)) ->
        let _ =
          Util.print_endline 1 "  Direction accepted";
        in Some(wh,d,h,bs,p)
      | (_,_,None) -> 
        let _ =
          Util.print_endline 1 "  Direction rejected";
        in find_direction mem cur_p t
      end

let find mem (dir:Direction.t) (hand:Hand.t) (cur_p:Point.t) =
    let dir_and_hands =

      let dir0 = dir in
      let dir1 = Direction.rotate dir0 1 in
      let dir2 = Direction.rotate dir1 1 in
      let dir3 = Direction.rotate dir2 1 in
      let hand0 = hand in
      let hand1 = Hand.switch hand in
      [dir0,hand0; dir0,hand1; dir1,hand1; dir1,hand0;
       dir2,hand0; dir2,hand1; dir3,hand1; dir3,hand0]
    in find_direction mem cur_p dir_and_hands


