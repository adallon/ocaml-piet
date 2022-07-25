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



 (*
let codel_map_size map = (Array.length map, Array.length map.(0))
let codel_map_example =
  let arr = codel_map_create 5 5 in
  let test_codel = Codel(Cyan,Light) in
  let _ =
    arr.(2).(2) <- test_codel ;
    arr.(2).(3) <- test_codel ;
    arr.(2).(1) <- test_codel ;
    arr.(3).(1) <- test_codel ;
    arr.(4).(1) <- test_codel ;
    arr.(4).(2) <- test_codel ;
    arr.(4).(3) <- test_codel ;
    arr.(4).(4) <- test_codel ;
    arr.(0).(0) <- test_codel ;
    arr.(1).(0) <- Black;(* White; *)
    arr.(0).(1) <- White;
    arr.(1).(1) <- White;
    arr.(2).(0) <- test_codel
  in arr
*)

let get_codel_block prog p =
  let _ =
    Util.print_endline 2 "    Computing color block"
  in
  let (map,group,_,_) = prog in 
  let _ = assert(IOR.element_at group p = None) in
  let codel = CR.element_at map p in

  let choice prog pij =
    (codel = CR.element_at map pij)
    && (undefined prog pij) && not(Point.equal pij p)

  in let candidates = choose choice prog
  
  in let rec find_close new_found remain_l target_l = function
    | [] -> new_found,remain_l
    | x::t -> 
        if Point.is_close x target_l 
        then find_close (x::new_found) remain_l target_l t
        else find_close new_found (x::remain_l) target_l t

  in let rec until_end candidates target =
    let new_found,remain_l = find_close [] [] target candidates
    in match new_found with
    | [] ->  target
    | _ -> until_end remain_l (List.rev_append new_found target)
      
  in let l = until_end candidates [p]
  in let _ =
    Util.print_string  2 "      Found: ";
    Util.print_endline 2 (String.concat ";" (List.map Point.to_string l));
  in l

