(** Type of parsed markdown *)

(** {1 Type definition} *)

type stack_elt = Em | Strong | EmStrong | Strike | Code

type span =
  | A of string
  | Br
  | CharSpan of char (* TODO remove? *)
  | CharsSpan of string
  | NoneSpan
  | StackOpen of stack_elt
  | StackClose of stack_elt
  | UnicodeSpan of string
  | UnsafeA of { spans : span list; (* TODO span? *) link : string }

type list_typ = Ordered | Unordered

type block =
  | CodeBlock of string list
  | H1 of span list
  | H2 of span list
  | H3 of span list
  | H4 of span list
  | H5 of span list
  | H6 of span list
  | Hr
  | List_ of list_typ * li list
  | P of span list
  | Quote of t
  | UnsafeCodeBlock of { cb : string list; classes : string list }
  | UnsafeImg of { alt : string; link : string; classes : string list }
  | UnsafeInlineHtml of string list

and li = Li of span list | LiP of block list
and t = block list

(** {1 Pretty printer} *)

val pp : ?rss:bool -> Format.formatter -> t -> unit
