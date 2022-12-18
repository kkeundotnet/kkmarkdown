(* SYNTAX: START *)
(** Br: e.g. [abc__<end-of-line>], where '[_]' is a space *)
(* SYNTAX: END *)

val construct : (string -> Typ.span list) -> string list -> Typ.span list
(** {!RuleBr.construct} is a special middle step between processing blocks to spans, in which it
    removes the markdown syntax for [<br>]. The first parameter is the [trans_spans] function that
    translates a markdown line without [<br>]. *)
