(** Inline HTML

Currently, it supports [div] and [script] only, but we are thinking of generalizing it to support {i
any} tags, since anyhow the inline html rule should be on only when unsafe mode. *)

module Div : BlockRule.S
(** {^ UNSAFE} Div inline HTML

{[
<div>
  ...
</div>
]} *)

module Script : BlockRule.S
(** {^ UNSAFE} Script inline HTML

{[
<script>
  ...
</script>
]} *)
