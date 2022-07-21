type t = int list

let to_string x = String.concat ";" (List.map string_of_int x)

let empty = []

let test1 = [0;1;2;5;7]
let test2 = [5;-3;2;12]

let push n (x:t) = n::x

let () =
  assert(push 6 empty = [6]);
  assert(push 7 test1 = [7;0;1;2;5;7])

let pop (x:t) =
  match x with
  | [] -> []
  | n::t -> t

let () =
  assert(pop empty = []);
  assert(pop test1 = [1;2;5;7]);
  assert(pop test2 = [-3;2;12])

let add (x:t) =
 match x with
 | a::(b::t) -> (a+b)::t
 | l -> l

let () =
  assert(add empty = []);
  assert(add [3] = [3]);
  assert(add test1 = [1;2;5;7]);
  assert(add test2 = [2;2;12])


let subst (x:t) =  
 match x with
 | a::(b::t) -> (b-a)::t
 | l -> l

let () =
  assert(subst empty = []);
  assert(subst [3] = [3]);
  assert(subst test1 = [1;2;5;7]);
  assert(subst test2 = [-8;2;12])

let mult (x:t) =  
 match x with
 | a::(b::t) -> (b*a)::t
 | l -> l

let () =
  assert(mult empty = []);
  assert(mult [3] = [3]);
  assert(mult test1 = [0;2;5;7]);
  assert(mult test2 = [-15;2;12])

let div (x:t) =  
 match x with
 | 0::t -> 0::t
 | a::(b::t) -> (b/a)::t
 | l -> l

let () =
  assert(div empty = []);
  assert(div [3] = [3]);
  assert(div test1 = [0;1;2;5;7]);
  assert(div test2 = [0;2;12])

let modulo (x:t) =  
  match x with
  | 0::t -> 0::t
  | a::(b::t) ->
     let x = if a>0 
     then  (a+(b mod a))   mod a
     else  ((b mod a) - a) mod a
     in x::t
  | l -> l

let () =
  assert(modulo empty = []);
  assert(modulo [3] = [3]);
  assert(modulo test1 = [0;1;2;5;7]);
  assert(modulo test2 = [2;2;12])

let not = function
  | [] -> []
  | a::t -> if a = 0 then 1::t else 0::t

let () =
  assert(not empty = []);
  assert(not [3] = [0]);
  assert(not test1 = [1;1;2;5;7]);
  assert(not test2 = [0;-3;2;12])

let greater = function
  | a::(b::t) -> if b > a then 1::t else 0::t
  | l -> l

let () =
  assert(greater empty = []);
  assert(greater [3] = [3]);
  assert(greater test1 = [1;2;5;7]);
  assert(greater test2 = [0;2;12])

let get (x:t) =
  (* pointer and switch will use this function *)
 match x with
 | n::t -> Some(n),t
 | [] -> None,[]

let () =
  assert(get empty = (None,[]));
  assert(get [3]   = (Some(3),[]));
  assert(get test1 = (Some(0),pop test1));
  assert(get test2 = (Some(5),pop test2))

let duplicate = function
  | a::t -> a::(a::t)
  | [] -> []

let () =
  assert(duplicate empty = []);
  assert(duplicate [3]   = [3;3]);
  assert(duplicate test1 = push 0 test1);
  assert(duplicate test2 = push 5 test2)

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


