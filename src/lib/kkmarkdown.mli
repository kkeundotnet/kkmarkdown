type t

val pp : ?rss:bool -> Format.formatter -> t -> unit

val trans : ?unsafe:bool -> string -> t

val trans_from_file : ?unsafe:bool -> string -> t

val trans_to_string : ?unsafe:bool -> ?rss:bool -> string -> string
