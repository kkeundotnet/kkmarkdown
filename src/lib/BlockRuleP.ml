let is_safe = true
let first_char = FirstChar.Any
let force_construct = true

type state = unit

let start line : state BlockRule.res =
  if Utils.is_empty_line line then Stop { state = (); handle_line = `Discard }
  else Go { state = (); handle_line = `Keep }

let continue () line = start line

let construct { BlockRule.trans_spans_from_lines } () lines =
  Typ.P (trans_spans_from_lines lines)
