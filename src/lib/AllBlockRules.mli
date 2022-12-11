(** Query for all block rules by first character *)

val find : char -> (module BlockRule.S) list
val any : (module BlockRule.S) list
