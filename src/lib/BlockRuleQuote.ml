let is_safe = true
let first_char = FirstChar.One '>'
let force_construct = true

type state = unit

let start line : state BlockRule.res =
  if String.starts_with ~prefix:"> " line then
    Go { state = (); handle_line = `Keep }
  else Die

let continue () line : state BlockRule.res =
  if Utils.is_empty_line line then Stop { state = (); handle_line = `Discard }
  else Go { state = (); handle_line = `Keep }

let re = Str.regexp "> ?\\|  ?"

let remove_indent line =
  if Str.string_match re line 0 then Str.string_after line (Str.match_end ())
  else line

let construct { BlockRule.trans_blocks } () lines =
  let lines = List.map remove_indent lines in
  Typ.Quote (trans_blocks lines)
