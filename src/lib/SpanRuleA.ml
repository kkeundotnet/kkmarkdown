module Automatic = struct
  let is_safe = true
  let first_char = FirstChar.One '<'
  let re = Str.regexp "<\\(https?://[^\"'>]*\\)>"

  let construct _ ({ SpanRule.s; cur } as state) : Typ.span option =
    if Str.string_match re s cur then (
      let link = Str.matched_group 1 s in
      state.cur <- Str.match_end ();
      Some (A link))
    else None
end

module UnsafeNormal = struct
  let is_safe = false
  let first_char = FirstChar.One '['
  let re = Str.regexp "\\[\\([^]]*\\)\\](\\([^)]*\\))"

  let construct trans_spans_1 ({ SpanRule.s; cur } as state) : Typ.span option =
    if Str.string_match re s cur then (
      let text = Str.matched_group 1 s in
      let link = Str.matched_group 2 s |> String.trim in
      state.cur <- Str.match_end ();
      Some (UnsafeA { spans = trans_spans_1 text; link }))
    else None
end
