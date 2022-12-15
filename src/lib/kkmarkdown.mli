val trans : ?unsafe:bool -> string -> Typ.t
val trans_from_file : ?unsafe:bool -> string -> Typ.t
val trans_from_stdin : ?unsafe:bool -> unit -> Typ.t
val trans_to_string : ?unsafe:bool -> ?rss:bool -> string -> string
