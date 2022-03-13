include module type of Stdlib.String

val forall : string -> f:(char -> bool) -> bool
val forall_from : int -> string -> f:(char -> bool) -> bool
val is_sub : int -> string -> sub:string -> bool
val is_prefix : string -> prefix:string -> bool
val sub_from : string -> int -> string
val index_sub_opt : string -> sub:string -> int option
val index_sub_from_opt : int -> string -> sub:string -> int option
val split_to_lines : string -> string list
