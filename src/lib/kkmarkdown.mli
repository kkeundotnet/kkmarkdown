(** Main entry for translation functions *)

val trans : ?unsafe:bool -> string -> Typ.t
(** @param unsafe Enable unsafe rules. False by default. *)

val trans_from_file : ?unsafe:bool -> string -> Typ.t
(** @param unsafe Enable unsafe rules. False by default. *)

val trans_from_stdin : ?unsafe:bool -> unit -> Typ.t
(** @param unsafe Enable unsafe rules. False by default. *)

val trans_to_string : ?unsafe:bool -> ?rss:bool -> string -> string
(** @param unsafe Enable unsafe rules. False by default.
@param rss Suppress the elements that are inappropriate for RSS. False by default. *)
