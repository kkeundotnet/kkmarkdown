let is_safe = true
let first_char = FirstChar.One '*'
let force_construct = true

type state = unit

let re = Str.regexp "\\*\\*\\*+[ \t]*$"

let start line : state BlockRule.res =
  if Str.string_match re line 0 then Stop { state = (); handle_line = `Discard }
  else Die

let continue _ _ =
  (* There is no [BlockRule.Go] state in [start], i.e. no continue. *)
  assert false

let construct _ () _ = Typ.Hr
