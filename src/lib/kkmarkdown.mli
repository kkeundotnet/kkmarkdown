type t

val pp : ?rss:bool -> Format.formatter -> t -> unit
val trans : ?unsafe:bool -> string -> Typ.block list
val trans_from_file : ?unsafe:bool -> string -> Typ.block list
val trans_from_stdin : ?unsafe:bool -> unit -> Typ.block list
val trans_to_string : ?unsafe:bool -> ?rss:bool -> string -> string
