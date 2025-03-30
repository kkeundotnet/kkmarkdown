(* SYNTAX: START *)
(** {^ UNSAFE} Image

    {[
      ![text](link) {.a .b}
    ]}

    is translated to

    {[
      <img alt="text" src="link" class="a b">
    ]}

    Note that the class part is optional. *)
(* SYNTAX: END *)

include BlockRule.S
