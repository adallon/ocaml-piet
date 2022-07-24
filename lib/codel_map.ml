type lightness = Light | Normal | Dark

let lightness_to_string = function
  | Light  -> "l"
  | Normal -> "n"
  | Dark   -> "d"

let lightness_val = function
  | Light  -> 0
  | Normal -> 1
  | Dark   -> 2

let lightness_diff a b = 
  (lightness_val b - lightness_val a + 3) mod 3

type hue = Red | Yellow | Green | Cyan | Blue | Magenta

let hue_val = function
  | Red     -> 0
  | Yellow  -> 1
  | Green   -> 2
  | Cyan    -> 3
  | Blue    -> 4
  | Magenta -> 5

let hue_to_string = function
  | Red     -> "R"
  | Yellow  -> "Y"
  | Green   -> "G"
  | Cyan    -> "C"
  | Blue    -> "B"
  | Magenta -> "M" 

let hue_diff a b = 
  (hue_val b - hue_val a + 6) mod 6

type codel = White | Black | Codel of hue * lightness

let codel_to_string = function
  | White -> " W"
  | Black -> " N" (* as Noir or Night *)
  | Codel (h,l) -> String.concat "" [lightness_to_string l ; hue_to_string h]

type memorized_data = bool * int * ((int*int) option)
type codel_map = 
  (codel array array) *
  (int option array array) *
  int ref *
  (int * Direction.direction * Direction.hand, memorized_data) Hashtbl.t

  (*
   *the codel array array represents the codel map
   *the int option array array represents the block numbers
   of the codels once it is set
   *the int represents the current max block number
   * the hashtable is used to memorize block info to avoid costly
   * computings.
   * It takes as input the block number, 
   * the DP and CC and returns the blocksize 
   * and the coordinate of the next point in this direction
   * if there is one
   *)

let print_coord_trace (x,y) =
  begin
    Util.print_string 0 "(";
    Util.print_int 0 x ;
    Util.print_string 0 ",";
    Util.print_int 0 y ;
    Util.print_string 0 ")"
  end

let codel_transition map (x0,y0) (x1,y1) =
  let map0,_,_,_ = map in
  let c1 = map0.(x0).(y0) in
  let c2 = map0.(x1).(y1) in
  let c1_string = codel_to_string c1 in
  let c2_string = codel_to_string c2 in
  let _ =
    Util.print_newline 0 ();
    print_coord_trace (x0,y0);
    Util.print_string 0 ":";
    Util.print_string 0 c1_string;
    Util.print_string 0 " -> ";
    print_coord_trace (x1,y1);
    Util.print_string 0 ":";
    Util.print_string 0 c2_string;
    Util.print_string 0 " ";
    (* Util.print_newline 0 (); *)
    (* next printing is the transition of the machine.
     * See function next_state in machine.ml
     * No endline necessary.
     *)
  in
  match c1,c2 with
  |White,_|_,White|Black,_|_,Black -> assert(false)
  (* no transition with a non-colored block *)
  |Codel(h0,l0),Codel(h1,l1) -> hue_diff h0 h1, lightness_diff l0 l1

let codel_black_white map x y = 
  let map0,_,_,_ = map in
  let maxX = Array.length map0 in
  let maxY = Array.length map0.(0) in
  let is_not_black = 
    x>=0 && x < maxX && y >= 0 && y < maxY && map0.(x).(y) != Black
  in let is_white =
    x>=0 && x < maxX && y >= 0 && y < maxY && map0.(x).(y) = White
  in is_not_black,is_white


let codel_map_create nx ny =
  let arr_map   = Array.make nx [||] in
  let arr_group = Array.make nx [||] in
  let rec aux = function
    | 0 -> ()
    | n -> 
        begin 
          arr_map.(n-1)   <- Array.make ny Black ; 
          arr_group.(n-1) <- Array.make ny None ;
          aux (n-1)
        end
  in let _ = aux nx in (arr_map,arr_group,ref 0,Hashtbl.create (nx*ny))

let codel_map_to_string codel_map =
  let map0,_,_,_ = codel_map in
  let indexesX = List.init (Array.length map0)     (fun i -> i) 
  in 
  let indexesY = List.init (Array.length map0.(0)) (fun i -> i) 
  in 
  let get_line y = 
    let l =
      List.map (fun x -> codel_to_string map0.(x).(y)) indexesX 
    in String.concat "." l
  in String.concat "\n" (List.map get_line indexesY)


let codel r g b =
  match r,g,b with
  | 255,255,255 -> White
  |   0,0,0     -> Black

  | 255,192,192 -> Codel(Red,Light)
  | 255,  0,  0 -> Codel(Red,Normal)
  | 192,  0,  0 -> Codel(Red,Dark)

  | 255,255,192 -> Codel(Yellow,Light)
  | 255,255,  0 -> Codel(Yellow,Normal)
  | 192,192,  0 -> Codel(Yellow,Dark)

  | 192,255,192 -> Codel(Green,Light)
  |   0,255,  0 -> Codel(Green,Normal)
  |   0,192,  0 -> Codel(Green,Dark)

  | 192,255,255 -> Codel(Cyan ,Light)
  |   0,255,255 -> Codel(Cyan ,Normal)
  |   0,192,192 -> Codel(Cyan ,Dark)

  | 192,192,255 -> Codel(Blue ,Light)
  |   0,  0,255 -> Codel(Blue ,Normal)
  |   0,  0,192 -> Codel(Blue ,Dark)

  | 255,192,255 -> Codel(Magenta, Light)
  | 255,  0,255 -> Codel(Magenta, Normal)
  | 192,  0,192 -> Codel(Magenta, Dark)

  |   _,  _,  _ -> White


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
   let c = codel r g b in let _ = map.(x).(y) <- c in ()
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

let get_codel_block map0 i j =
  let (map,_,_,_) = map0 in 
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

let next_cases map dir hand (cX,cY) =
  let dir_and_hands = Direction.dir_hand_order dir hand in
  let map0,group,maxG,tab = (map:codel_map) in

  let get_next_coord wh d (x,y) =
    let rec aux (x0,y0) wh (x,y) =
      let not_black,white = codel_black_white map x y in
      if white
      (* the codel is white: we continue until we find a border,
       * a colored block or a black codel *)
      then aux (x0,y0) true (Direction.next_point (x,y) d)
      else if not_black
      (* it is neither white nor black: it is a colored block *)
      then wh,Some(x,y)
      else (* it is black or a border *)
        if wh = false ||(x0=x && y0 = y)
        (* We were never at a white block, 
         * or the direction is completely obstructed.
         *)
        then wh,None 
        else true,Some(x0,y0)
    in aux (x,y) wh (x,y) 
  
  in let get_possibility (d,h) = 
    let _ =
      Util.print_string 1 "Current direction ";
      Util.print_endline 1 (Direction.direction_to_string d); 
      Util.print_string 1 "Current hand ";
      Util.print_endline 1 (Direction.hand_to_string h); 
    in match map0.(cX).(cY),group.(cX).(cY) with
    | White,Some(g) ->
        let _ =
          Util.print_endline 1 "We are at a white codel";
          Util.print_endline 1 "  (Direction seen before)";
        in
        if Hashtbl.mem tab (g,d,h)
        then Hashtbl.find tab (g,d,h)
        else let wh,next_p = 
          get_next_coord true d (Direction.next_point (cX,cY) d)
        in let _ = Hashtbl.add tab (g,d,h) (wh,0,next_p)
        in (wh,0,next_p)
        (* = Hashtbl.find tab (g,d,h) *)
    | White,None ->
        let _ =
          Util.print_endline 1 "We are at a white codel";
          Util.print_endline 1 "  (Not seen yet)";
        in
        let gVal = !maxG in
        let    _ = maxG:=!maxG+1 ; group.(cX).(cY) <- Some(gVal) in
        let wh,next_p =
          get_next_coord true d (Direction.next_point (cX,cY) d)
        in let _ = Hashtbl.add tab (gVal,d,h) (wh,0,next_p)
        in (wh,0,next_p)
        (* = Hashtbl.find tab (g,d,h) *)
    | _,Some(g) ->
        let _ =
          Util.print_endline 1 "We are at a color codel";
          Util.print_endline 1 "  (Seen before)";
        in
        if Hashtbl.mem tab (g,d,h)
        then Hashtbl.find tab (g,d,h)
        else 
          let color_block = get_codel_block map cX cY in
          let blocksize = List.length color_block in
          let f_list = Direction.furthest color_block d in
          let cc_dir = Direction.rotate_hand d h in
          let outside_point = 
            (* points to go out of the block,
            * with no consideration of the possibility of it.
            * We examine now if they are inside the map,
            * if they are black/white.
            * Note that outside cases are handled as black *)
            match Direction.furthest f_list cc_dir with
            | [x1,y1] -> Direction.next_point (x1,y1) d
            | _ -> assert(false) 
            (* only one element at this point :
             * we are at a border of an edge. *)
          in let wh,next_p = 
            get_next_coord false d outside_point
        in let _ = Hashtbl.add tab (g,d,h) (wh,0,next_p)
        in (wh,blocksize,next_p)
    | _,None ->
        let _ =
          Util.print_endline 1 "We are at a color codel";
          Util.print_endline 1 "  (Not seen yet)";
        in
        let color_block = get_codel_block map cX cY in
        let blocksize = List.length color_block in
        let gVal = !maxG in
        let _ =
          let rec aux = function
            | [] -> maxG := !maxG+1
            | (x,y)::t ->
                begin group.(x).(y) <- Some(gVal) ; aux t end
          in aux color_block
        in 
        let f_list = Direction.furthest color_block d in
        let cc_dir = Direction.rotate_hand d h in
        let outside_point = 
          (* points to go out of the block,
           * with no consideration of the possibility of it.
           * We examine now if they are inside the map,
           * if they are black/white.
           * Note that outside cases are handled as black *)
          match Direction.furthest f_list cc_dir with
          | [x1,y1] -> Direction.next_point (x1,y1) d
          | _ -> assert(false) 
          (* only one element at this point :
           * we are at a border of an edge. *)
        in let (wh,next_p) = 
          get_next_coord false d outside_point
        in let _ = 
          Hashtbl.add tab (gVal,d,h) (wh,blocksize,next_p)
        in ((wh,blocksize,next_p):memorized_data)
        (* = Hashtbl.find tab (g,d,h) *)

    in let rec find_direction = function
      | [] -> 
          let _ =
            Util.print_endline 1 "No direction has been found. Execution will terminate."
          in
          None
      | (d,h)::t -> 
          let _ =
            Util.print_string 1 "Trying with direction ";
            Util.print_string 1 (Direction.direction_to_string d); 
            Util.print_string 1 " and hand ";
            Util.print_endline 1 (Direction.hand_to_string h); 
          in
          begin match get_possibility (d,h) with
          | (wh,bs,Some(x,y)) ->
            let _ =
              Util.print_endline 1 "Direction accepted";
            in Some(wh,d,h,bs,x,y)
          | (_,_,None) -> 
            let _ =
              Util.print_endline 1 "Direction rejected";
              in find_direction t
          end
    in find_direction dir_and_hands


let init_state map = (map,(0,0),Machine.init_machine)

let explorator state =
  let map,(cX,cY),machine = state in
  let dir  = Machine.get_direction machine in
  let hand = Machine.get_hand machine in
  match next_cases map dir hand (cX,cY) with
  | None -> None
  | Some(wh,d,h,bs,x,y) -> 
      let new_state = (map,(x,y),Machine.set d h machine)
      in let _ = 
        if Util.step_by_step 
        then let _ = print_string ">"; read_line () in ()
        else ()
      in Some(new_state,bs,wh)

let interpreter map =
  let rec aux state =
    let (_,coord0,_) = state in 
    let trans_opt = explorator state in
    match trans_opt with
    | None -> Util.print_string 0 "\nEnd of execution\n"
    | Some(state1,blocksize,wh) ->
        let (_,coord1,mach1) = state1 in
        let th,tl =
          if wh then (0,0)
          else codel_transition map coord0 coord1 
        in let inst  = Instructions.transition th tl
        in let mach2 = Machine.next_state mach1 blocksize inst
        in aux (map,coord1,mach2)
  in let state0 = init_state map 
  in aux state0

                


