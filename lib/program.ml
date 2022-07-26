open Geometry

module CR = Rectangle(Codel)

module IOR = 
  Rectangle (struct 
    type t = int option 
  end)

type t = CR.t * IOR.t * int ref * Hashmemory.t

  (*
   *the codel rectangle represents the codel map
   *the int option rectangle represents the block numbers
   of the codels once it is set
   *the int represents the current max block number
   * the hashmemory is used to memorize block info to avoid costly
   * computings.
   * See hashmemory.ml
   *)
let inside (m,_,_,_) = CR.inside m
let codel_at (m,_,_,_) = CR.element_at m
let sizeX (m,_,_,_) = CR.sizeX m
let sizeY (m,_,_,_) = CR.sizeY m
let undefined (_,g,_,_) p = (IOR.element_at g p = None)
let choose f prog =
  let rec line res i = function
    | 0 -> res
    | j ->
        let pij = Point.to_point  (i,j-1) in 
        if f prog pij then line (pij::res) i (j-1) else line res i (j-1)
  in let rec all_vals res = function
    | 0 -> res
    | i -> all_vals (line res (i-1) (sizeY prog)) (i-1) 
  in all_vals [] (sizeX prog)


let create nx ny =
  let cmap =  CR.create Codel.Black nx ny in
  let gmap = IOR.create None        nx ny in
  (cmap,gmap,ref 0,Hashmemory.create (nx*ny))

let to_string codel_map =
  let map0,_,_,_ = codel_map in
  let indexesX = List.init (CR.sizeX map0) (fun i -> i) in 
  let indexesY = List.init (CR.sizeY map0) (fun i -> i) in 
  let get_line y = 
    let f x =
      Codel.to_string (CR.element_at map0 (Point.to_point (x,y)))
    in let l =
      List.map f indexesX 
    in String.concat "." l
  in String.concat "\n" (List.map get_line indexesY)

let of_png fpath =
 let ich = open_in fpath in
 let rec read_file res ic =
  try let l = input_line ic in read_file (l::res) ic
  with
  | End_of_file -> String.concat "\n" (List.rev res)
 in let file_as_str = read_file [] ich
 in let _ = close_in ich 
 in let img = ImagePNG.parsefile (ImageUtil.chunk_reader_of_string file_as_str)
 in let (map,group,maxG,tab) = create img.width img.height
 in let f x y r g b =
   let c = Codel.of_rgb r g b 
   in let _ = CR.set map (Point.to_point (x,y)) c 
   in ()
 in let g p _ = 
   let x,y = Point.x p, Point.y p in Image.read_rgb img x y (f x y) 
 in let _ = CR.iter g map
 in (map,group,maxG,tab)

let get_codel_block prog p =
  let _ = Util.print_endline 2 "    Computing color block" in
  let (map,group,_,_) = prog in 
  let _ = assert(IOR.element_at group p = None) in
  let codel = CR.element_at map p in

  let choice prog pij =
    (codel = CR.element_at map pij)
    && (undefined prog pij) && not(Point.equal pij p)

  in let candidates = choose choice prog
  in let _ =
    let l_str = List.map Point.to_string candidates in
    Util.print_string 3 "candidates: ";
    Util.print_endline 3 (String.concat ";" l_str)
  
  in let rec find_close new_found remain_l target_l = function
    | [] -> new_found,remain_l
    | x::t -> 
        if Point.is_close x target_l 
        then find_close (x::new_found) remain_l target_l t
        else find_close new_found (x::remain_l) target_l t

  in let rec until_end candidates target =
    let _ =
      let l_str = List.map Point.to_string candidates in
      Util.print_string 3 "target: ";
      Util.print_endline 3 (String.concat ";" l_str)
    in let new_found,remain_l = find_close [] [] target candidates
    in match new_found with
    | [] ->  target
    | _ -> until_end remain_l (List.rev_append new_found target)
      
  in let l = until_end candidates [p]
  in let _ =
    Util.print_string  2 "      Found: ";
    Util.print_endline 2 (String.concat ";" (List.map Point.to_string l));
  in l

let get_next_coord prog wh d p =
  (* prog: program
   * wh: seen white codel?
   * d : last direction?
   * p : point *)
  let map0,_,_,_ = prog in
  let rec aux p0 wh p =
    let black,white =
      if inside prog p 
      then 
        let c = CR.element_at map0 p in
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

let get_possibility prog cur_p (d,h) =
  let  (map,group,maxG,tab) = prog in
  let _ =
    Util.print_string 1 "  Current direction ";
    Util.print_endline 1 (Direction.to_string d); 
    Util.print_string 1 "  Current hand ";
    Util.print_endline 1 (Hand.to_string h); 
  in let c,g_op = 
    (CR.element_at  map  cur_p), (IOR.element_at group cur_p)
  in match c, g_op  with
  | Codel.White,Some(_) ->
    let _ =
      Util.print_endline 1 "  We are at a white codel";
      Util.print_endline 1 "    (Seen before)";
    in let wh,next_p = 
      get_next_coord prog true d (Direction.next_point cur_p d)
    in (wh,0,next_p)
  | Codel.White,None ->
    let _ =
      Util.print_endline 1 "  We are at a white codel";
      Util.print_endline 1 "    (Not seen yet)";
    in
    let gVal = !maxG in
    let    _ = 
      maxG:=!maxG+1 ; 
      IOR.set group cur_p (Some gVal)
    in
    let wh,next_p =
      get_next_coord prog true d (Direction.next_point cur_p d)
    (* in let _ = Hashtbl.add tab (gVal,d,h) (wh,0,next_p) *)
    in (wh,0,next_p)
    (* = Hashtbl.find tab (g,d,h) *)
  |Codel.Black,_ -> assert(false) (* we cannot be at a black codel *)
  | _,Some(g) ->      
    let _ =
        Util.print_endline 1 "  We are at a color codel";
        Util.print_endline 1 "    (Color block seen before)";
    in let size,corner = Hashmemory.get_corner tab g d h
    in let wh,next_p = 
        get_next_coord prog false d (Direction.next_point corner d)
      in (wh,size,next_p)
  | _,None ->
      let _ =
        Util.print_endline 1 "  We are at a color codel";
        Util.print_endline 1 "    (Color block not seen yet)";
      in
      let color_block = get_codel_block prog cur_p in
      let blocksize = List.length color_block in
      let gVal = !maxG in
      let _ =
        let rec aux = function
          | [] -> maxG := !maxG+1
          | p::t ->
              begin IOR.set group p (Some gVal); aux t end

        in aux color_block
      in 
      let _ = Hashmemory.add_group tab gVal color_block blocksize in
      let size,corner = Hashmemory.get_corner tab gVal d h in
      let _ = assert(size = blocksize) in
      let (wh,next_p) = get_next_coord prog false d (Direction.next_point corner d)
      in (wh,size,next_p)


let rec find_direction prog cur_p = function
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
      in begin match get_possibility prog cur_p (d,h) with
      | (wh,bs,Some(p)) ->
        let _ =
          Util.print_endline 1 "  Direction accepted";
        in Some(wh,d,h,bs,p)
      | (_,_,None) -> 
        let _ =
          Util.print_endline 1 "  Direction rejected";
        in find_direction prog cur_p t
      end

let next_codel (prog:t) (dir:Direction.t) (hand:Hand.t) (cur_p:Point.t) =
    let dir_and_hands =

      let dir0 = dir in
      let dir1 = Direction.rotate dir0 1 in
      let dir2 = Direction.rotate dir1 1 in
      let dir3 = Direction.rotate dir2 1 in
      let hand0 = hand in
      let hand1 = Hand.switch hand in
      [dir0,hand0; dir0,hand1; dir1,hand1; dir1,hand0;
       dir2,hand0; dir2,hand1; dir3,hand1; dir3,hand0]
    in find_direction prog cur_p dir_and_hands


