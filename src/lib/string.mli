include module type of Stdlib.String

val forall : string -> f:(char -> bool) -> bool

val forall_from : int -> string -> f:(char -> bool) -> bool

val is_sub : int -> string -> sub:string -> bool

val is_prefix : string -> prefix:string -> bool

val sub_from : string -> int -> string
