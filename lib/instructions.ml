type instr = 
  | Push      | Pop (* basic stack *)   
  | Add       | Substract | Multiply | Divide    | Mod (* Basic arithmetic *)
  | Not       | Greater (* Logic  *)    
  | Pointer   | Switch  (* Arrows *)
  | Duplicate | Roll    (* Advanced stack *)  
  | InInt     | InChar  (* io : input  *)  
  | OutInt    | OutChar (* io : output *)

module Stack = struct
  type t = int list

  let empty = []

  let push n (x:t) = n::x
  let pop (x:t) =
    match x with
    | [] -> []
    | n::t -> t
  
  let add (x:t) =
   match x with
   | a::(b::t) -> (a+b)::t
   | l -> l

  let subst (x:t) =  
   match x with
   | a::(b::t) -> (b-a)::t
   | l -> l

  let mult (x:t) =  
   match x with
   | a::(b::t) -> (b*a)::t
   | l -> l

  let div (x:t) =  
   match x with
   | 0::t -> 0::t
   | a::(b::t) -> (b/a)::t
   | l -> l

  let modulo (x:t) =  
    match x with
    | 0::t -> 0::t
    | a::(b::t) ->
       let x = if a>0 
       then  (a+(b mod a))   mod a
       else  ((b mod a) - a) mod a
       in x::t
    | l -> l

  let not = function
    | [] -> []
    | a::t -> if a = 0 then 1::t else 0::t

  let greater = function
    | a::(b::t) -> if b > a then 1::t else 0::t
    | l -> l

  let get (x:t) =
    (* pointer and switch will use this function *)
   match x with
   | n::t -> Some(n),t
   | [] -> None,[]

  let duplicate x = function
    | a::t -> a::(a::t)
    | [] -> []

  let roll_aux nrolls depth x =
    let rec get_firsts n res = function
      | l when n = 0 -> Some(res,l)
      | [] -> None
      | a::t -> get_firsts (n-1) (a::res) t
    in let nrolls = (depth + (nrolls mod depth) mod depth)
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
    | a::(b::t) -> roll_aux a b t
    | t -> t

  let inint x =
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
          let _ = print_char (Char.chr x) in t
        else x::t
    | [] -> []
end

