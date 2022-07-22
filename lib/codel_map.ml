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

let codel_transition map (x0,y0) (x1,y1) =
  let c1 = map.(x0).(y0) in
  let c2 = map.(x1).(y1) in
  match c1,c2 with
  |White,_|_,White|Black,_|_,Black -> assert(false)
  (* no transition with a non-colored block *)
  |Codel(h0,l0),Codel(h1,l1) -> hue_diff h0 h1, lightness_diff l0 l1

type codel_map = codel array array

let codel_black_white map x y = 
  let maxX = Array.length map in
  let maxY = Array.length map.(0) in
  let is_not_black = 
    x>=0 && x < maxX && y >= 0 && y < maxY && map.(x).(y) != Black
  in let is_white =
    x>=0 && x < maxX && y >= 0 && y < maxY && map.(x).(y) = White
  in is_not_black,is_white



let codel_map_to_string codel_map =
  let indexesX = List.init (Array.length codel_map)     (fun i -> i) 
  in 
  let indexesY = List.init (Array.length codel_map.(0)) (fun i -> i) 
  in 
  let get_line y = 
    let l =
      List.map (fun x -> codel_to_string codel_map.(x).(y)) indexesX 
    in String.concat "." l
  in String.concat "\n" (List.map get_line indexesY)

let codel_map_size map = (Array.length map, Array.length map.(0))

let codel_map_example =
  let arr = Array.make 5 [||] in
  let rec aux = function
    | 0 -> ()
    | n -> arr.(n-1) <- Array.make 5 Black ; aux (n-1)
  in let _ = aux 5 in
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
    arr.(0).(0) <- test_codel
  in arr

let get_codel_block map i j =
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
