type t

val pp : Format.formatter -> t -> unit

val trans : string -> t

val trans_from_strings : string list -> t

val trans_to_string : string -> string
