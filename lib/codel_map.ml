open Geometry

type t = 
  (Codel.t array array) *
  (int option array array) * 
  int ref * Hashmemory.t

  (*
   *the codel array array represents the codel map
   *the int option array array represents the block numbers
   of the codels once it is set
   *the int represents the current max block number
   * the hashmemory is used to memorize block info to avoid costly
   * computings.
   * See hashmemory.ml
   *)

let codel_black_white map x y = 
  let map0,_,_,_ = map in
  let maxX = Array.length map0 in
  let maxY = Array.length map0.(0) in
  let is_not_black = 
    x>=0 && x < maxX && y >= 0 && y < maxY && map0.(x).(y) != Codel.Black
  in let is_white =
    x>=0 && x < maxX && y >= 0 && y < maxY && map0.(x).(y) = Codel.White
  in is_not_black,is_white


let codel_map_create nx ny =
  let arr_map   = Array.make nx [||] in
  let arr_group = Array.make nx [||] in
  let rec aux = function
    | 0 -> ()
    | n -> 
        begin 
          arr_map.(n-1)   <- Array.make ny Codel.Black ; 
          arr_group.(n-1) <- Array.make ny None ;
          aux (n-1)
        end
  in let _ = aux nx in (arr_map,arr_group,ref 0,Hashmemory.create (nx*ny))

let codel_map_to_string codel_map =
  let map0,_,_,_ = codel_map in
  let indexesX = List.init (Array.length map0)     (fun i -> i) 
  in 
  let indexesY = List.init (Array.length map0.(0)) (fun i -> i) 
  in 
  let get_line y = 
    let l =
      List.map (fun x -> Codel.to_string map0.(x).(y)) indexesX 
    in String.concat "." l
  in String.concat "\n" (List.map get_line indexesY)

let png_to_map fpath =
 let ich = open_in fpath in
 let rec read_file res ic =
  try let l = input_line ic in read_file (l::res) ic
  with
  | End_of_file -> String.concat "\n" (List.rev res)
 in let file_as_str = read_file [] ich
 (* in let _ = close_in ich *)
 in let img = ImagePNG.parsefile (ImageUtil.chunk_reader_of_string file_as_str)
 in let (map,group,maxG,tab) = 
   codel_map_create img.width img.height
 in let f x y r g b =
   let c = Codel.of_rgb r g b in let _ = map.(x).(y) <- c in ()
 in let g x y = Image.read_rgb img x y (f x y) 
 in let rec apply_g_x x = function
   | 0 -> ()
   | n -> begin g x (n-1) ; apply_g_x x (n-1) end
 in let rec apply_g = function
   | 0 -> ()
   | n -> begin apply_g_x (n-1) img.height ; apply_g (n-1) end
 in let _ = apply_g img.width
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

let get_codel_block map0 p =
  let _ =
    Util.print_endline 2 "    Computing color block"
  in
  let (map,group,_,_) = map0 in 
  let x,y = Point.x p,Point.y p in
  let _ = assert(group.(x).(y) = None) in
  let codel = map.(x).(y) in
  let xMax = Array.length map in
  let yMax = Array.length map.(0) in
  let rec line res i = function
    | 0 -> res
    | j -> 
        if (codel = map.(i).(j-1)) 
        && (group.(i).(j-1) = None) && 
        (i != x || j-1!= y) 
           then line ((Point.to_point (i,j-1))::res) i (j-1)
           else line res i (j-1)
  in let rec all_vals res = function
    | 0 -> res
    | i -> all_vals (line res (i-1) yMax) (i-1) 
  in let candidates = all_vals [] xMax 
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
  (*
  let explored  = [(i,j)] in

  let add_if_not_in seen new_xplr a =
    if List.mem a seen
    then new_xplr
    else a::new_xplr
  
  in let adj (i,j) =
    let lx = Array.length map in
    let ly = Array.length map.(0) in
    let adj_l = [(i+1,j);(i-1,j);(i,j+1);(i,j-1)] in
    let rec remove_outside = function
      | [] -> []
      | (a,b)::t when a < 0 || a >= lx || b<0 || b>= ly -> 
          remove_outside t
      | (a,b)::t when map.(a).(b) = map.(i).(j) -> 
          (a,b)::(remove_outside t)
      | (_,_)::t -> remove_outside t 
    in remove_outside adj_l
  
  in let rec explorator seen new_exploring = function
    | [] -> (seen,new_exploring)
    | a::l -> 
        let adj_a = adj a in 
        let new_exploring = List.fold_left (add_if_not_in seen) new_exploring adj_a
        in explorator (if List.mem a seen then seen else a::seen) new_exploring l
  in let rec aux explored exploring =
    let explored,exploring = explorator explored [] exploring
    in match exploring with
    | [] -> explored
    | exploring -> aux explored exploring
  in aux explored explored
  *)

