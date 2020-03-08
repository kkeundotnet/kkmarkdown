type t

val pp : Format.formatter -> t -> unit

val trans_lines : string list -> t

val trans : string -> t
