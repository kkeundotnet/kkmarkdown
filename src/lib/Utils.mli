(** Utilities *)

val is_empty_line : string -> bool
(** Check is the string is empty, i.e. consists of spaces or tabs *)

val re_spaces_upto_four : Str.regexp
(** Regular expression for sequent spaces upto four *)

val read_classes : string -> string list
(** Read classes annotation, e.g. [".a .b"] is read to [\["a"; "b"\]] *)

val remove_trailing_spaces : string -> string
val remove_trailing_sharps : string -> string
