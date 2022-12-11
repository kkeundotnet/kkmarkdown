(** Header *)

module Sharp : BlockRule.S
(** Headers: e.g. [# abc] is for [<h1>] and [## abc] is for [<h2>]. Similar rules are applied for
    [<h3>] to [<h6>]. *)

module H1 : BlockRule.S
(** Header [<h1>] by underline

{[
Title
===
]} *)

module H2 : BlockRule.S
(** Header [<h2>] by underline

{[
Sub-title
---
]} *)
