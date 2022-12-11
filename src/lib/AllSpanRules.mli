(** Query for all span rules by first character *)

val find : char -> (module SpanRule.S) list
val any : (module SpanRule.S) list
