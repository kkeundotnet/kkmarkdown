module F = Format

let is_safe = true
let first_char = FirstChar.OneOf [ '*'; '-' ]
let force_construct = true

type state = unit

let re char =
  Str.regexp (F.sprintf "%s[ \t]*%s[ \t]*%s[%s \t]*$" char char char char)

let re_star = re "\\*"
let re_dash = re "-"

let start line : state BlockRule.res =
  if Str.string_match re_star line 0 || Str.string_match re_dash line 0 then
    Stop { state = (); handle_line = `Discard }
  else Die

let continue _ _ =
  (* There is no [BlockRule.Go] state in [start], i.e. no continue. *)
  assert false

let construct _ () _ = Typ.Hr
