(** Span-rules that is based on stack *)

(* SYNTAX: START *)

module Em : SpanRule.S
(** Emphasis: e.g. [*abc*] *)

module Strong : SpanRule.S
(** Strong: e.g. [**abc**] *)

module EmStrong : SpanRule.S
(** Emphasis+strong: e.g. [***abc***] *)

module Strike : SpanRule.S
(** Strike: e.g. [~~abc~~] *)

module Code : SpanRule.S
(** Code: e.g. [`abc`] *)

(* SYNTAX: END *)
