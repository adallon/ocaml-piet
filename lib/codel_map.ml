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
  | White -> "W "
  | Black -> "N " (* as Noir or Night *)
  | Codel (h,l) -> String.concat "" [hue_to_string h ; lightness_to_string l]

type memorized_data = int * ((int*int) option)
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

let codel_transition map (x0,y0) (x1,y1) =
  let map0,_,_,_ = map in
  let _ =
    Util.print_endline "Transition:";
    (* print_int x0; print_string ","; print_int y0 ; 
    print_string " -> ";
    print_int x1; print_string ","; print_int y1 ; 
    print_newline (); *)
  in

  let c1 = map0.(x0).(y0) in
  let c2 = map0.(x1).(y1) in
  (*
  let _ = print_endline (codel_to_string c1) in
  let _ = print_endline (codel_to_string c2) in
  *)
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

let get_codel_block (map:codel array array) i j =
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
  let map0,group,maxG,tab = map in
  let possibilities = 
    match group.(cX).(cY) with
    | Some(g) -> 
      List.map (fun (d,h) -> (d,h,Hashtbl.find tab (g,d,h))) dir_and_hands
    | None ->
        let color_block = get_codel_block map0 cX cY in
        let blocksize = List.length color_block in
        let gVal = !maxG in
        let _ =
          let rec aux = function
            | [] -> maxG := !maxG+1
            | (x,y)::t ->
                begin group.(x).(y) <- Some(gVal) ; aux t end
          in aux color_block
        in let to_map (d,h) =
          let f_list = Direction.furthest color_block d in
          let cc_dir = Direction.rotate_hand d h in
          match Direction.furthest f_list cc_dir with
          | [x1,y1] -> (d,h,Direction.next_point (x1,y1) d)
          | _ -> assert(false) 
          (* only one element at this point :
           * we are at a border of an edge    *)
        in let outside_points = List.map to_map dir_and_hands
          (* points to go out of the block,
           * with no consideration of the possibility of it.
           * We examine now if they are inside the map,
           * if they are black/white.
           * Note that outside cases are handled as black *)
        in let rec get_next_coord = function
          | (d,h,(x,y)) ->
              let not_black,white =
                codel_black_white map x y in
              if white
              then get_next_coord (d,h,Direction.next_point (x,y) d)
              else if not_black
              then d,h,Some(x,y)
              else d,h,None
        in let next_points_computed =
          List.map get_next_coord outside_points
        in let rec hash_update = function
          | [] -> ()
          | (d,h,o)::t -> Hashtbl.add tab (gVal,d,h) (blocksize,o); hash_update t
        in let _ = hash_update next_points_computed
        in 
        List.map (fun (d,h) -> (d,h, Hashtbl.find tab (gVal,d,h))) dir_and_hands
    in let rec handle_possibilities = function
      | [] -> None
      | (_,_,(_,None))::t -> handle_possibilities t
      | (d,h,(bs,Some(x,y)))::_ -> Some(d,h,bs,x,y)
    in handle_possibilities possibilities


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

                


