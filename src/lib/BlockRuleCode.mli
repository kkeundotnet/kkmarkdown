(** Code block *)

(* SYNTAX: START *)

module Backquote : BlockRule.S
(** Code block with backquote

{[
```
abc
```
]} *)

module Tilde : BlockRule.S
(** Code block with tilde

{[
~~~
abc
~~~
]} *)

module UnsafeBackquote : BlockRule.S
(** {^ UNSAFE} Code block with backquote

{[
``` {.abc}
abc
```
]} *)

module UnsafeTilde : BlockRule.S
(** {^ UNSAFE} Code block with tilde

{[
~~~ {.abc}
abc
~~~
]} *)

module Indent : BlockRule.S
(** Code block with indent

{[
____abc
]}

where [_] is a space *)

(* SYNTAX: END *)
