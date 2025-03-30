(** Inline HTML

    Currently, it supports [div] and [script] only, but we are thinking of
    generalizing it to support {i any} tags, since anyhow the inline html rule
    should be on only when unsafe mode. *)

(* SYNTAX: START *)

module Div : BlockRule.S
(** {^ UNSAFE} Div inline HTML

    {[
      <div>
        ...
      </div>
    ]}

    Note that [<div>] and [</div>] should be their own lines. *)

module Script : BlockRule.S
(** {^ UNSAFE} Script inline HTML

    {[
      <script>
        ...
      </script>
    ]} *)

(* SYNTAX: END *)
