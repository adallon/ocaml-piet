open Geometry

type memorized_data = 
  Point.t list (* list of codel coordinates of the group *)
  * int (* size of the list *)
  * (Direction.t, Point.t * Point.t ) Hashtbl.t 
  (* direction, left corner, right corner *)

type t = (int , memorized_data) Hashtbl.t

let create n = Hashtbl.create n

let add_group tab g block size =
  Hashtbl.add tab g (block,size,Hashtbl.create 4)

(*
  let get_group_size tab g =
  let _,bs,_ = Hashtbl.find tab g in bs
  *)

let get_corner tab g d h =
  if Hashtbl.mem tab g
  then
    (* The group has been set *)
    begin
    let (block,size,corner_tab) = Hashtbl.find tab g in
    if Hashtbl.mem corner_tab d
    then
      let (left,right) = Hashtbl.find corner_tab d in 
      match h with
      | Hand.Left  -> size,left
      | Hand.Right -> size,right
    else
      let edge = 
        Direction.furthest block d 
      in let left_dir  = Direction.rotate d (-1)
      in let right_dir = Direction.rotate d ( 1)
      in let (left_point,right_point) = 
        let l = 
          Direction.furthest edge left_dir  
        in
        let r = 
          Direction.furthest edge right_dir 
        in
        match l,r with
            | [coord_l],[coord_r] -> coord_l,coord_r
            | _,_ -> assert(false) 
            (* only one element at this point :
             * we are at a border of an edge. *)
      in let _ = Hashtbl.add corner_tab d (left_point,right_point)
      in match h with
      | Hand.Left -> size,left_point
      | Hand.Right -> size,right_point
    end
  else assert(false)
    (* Group has not been set yet: we should not use the function
      * in this case *)


