type t

val pp : Format.formatter -> t -> unit

val trans_from_string_list : string list -> t

val trans : string -> t
