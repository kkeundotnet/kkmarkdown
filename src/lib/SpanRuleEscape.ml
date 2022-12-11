let is_safe = true
let first_char = FirstChar.One '\\'
let re = Str.regexp "\\\\\\([]\\\\`\\*_{}[()#\\+\\.!-]\\)"

let construct _ ({ SpanRule.s; cur } as state) =
  if Str.string_match re s cur then (
    let c = Str.matched_group 1 s in
    state.cur <- Str.match_end ();
    Some (Typ.CharSpan c.[0]))
  else None
