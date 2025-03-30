(* SYNTAX: START *)
(** Escaped character: HTML special characters, e.g. [&], [<], etc., are
    translated to [&amp;], [&lt;], etc. The following characters should be
    escaped by backslash in markdown.

    {[
      [ ] \ ` * # _ { } ( ) + - . ! ~
    ]} *)
(* SYNTAX: END *)

include SpanRule.S
