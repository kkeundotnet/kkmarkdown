(** Link *)

(* SYNTAX: START *)

module Automatic : SpanRule.S
(** Link: e.g. [<https://kkeun.net>]

For simplicity, the [https://] prefix is omitted in the translated result, i.e. the example above is
translated to

{[
<a href="https://kkeun.net">kkeun.net</a>
]}

Note that the link address must start with [https://] or [http://]. *)

module UnsafeNormal : SpanRule.S
(** {^ UNSAFE} Link: e.g. [\[kkeundotnet\](https://kkeun.net)] *)

(* SYNTAX: END *)
