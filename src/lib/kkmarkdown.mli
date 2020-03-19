type t

val pp : Format.formatter -> t -> unit

val trans : ?unsafe:bool -> string -> t

val trans_to_string : string -> string
