(** Span-rules that is based on stack *)

(* SYNTAX: START *)

module Em : SpanRule.S
(** Emphasis: e.g. [*abc*] or [_abc_] *)

module Strong : SpanRule.S
(** Strong: e.g. [**abc**] or [__abc__] *)

module EmStrong : SpanRule.S
(** Emphasis+strong: e.g. [***abc***] or [___abc___]

    Note that nested forms of emphasis and strong are NOT supported. For
    example,

    {[
      ***word*word**
    ]}

    will NOT be translated as you expect. *)

module Strike : SpanRule.S
(** Strike: e.g. [~~abc~~] *)

module Code : SpanRule.S
(** Code: e.g. [`abc`] *)

(* SYNTAX: END *)
