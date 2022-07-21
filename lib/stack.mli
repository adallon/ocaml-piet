type t
val empty:t
val push: int -> t -> t 
val pop     : t -> t
val add     : t -> t
val subst   : t -> t
val mult    : t -> t
val div     : t -> t
val modulo  : t -> t
val not     : t -> t
val greater : t -> t
val get     : t -> int option * t 
val duplicate : t -> t
val roll    : t -> t
val inint   : t -> t 
val inchar  : t -> t 
val outint  : t -> t 
val outchar : t -> t 
val to_string : t -> string

