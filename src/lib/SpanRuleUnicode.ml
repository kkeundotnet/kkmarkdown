module Make (M : sig
  val re : Str.regexp
end) =
struct
  let is_safe = true
  let first_char = FirstChar.One '&'

  let construct _ ({ SpanRule.s; cur } as state) =
    if Str.string_match M.re s cur then (
      state.cur <- Str.match_end ();
      Some (Typ.UnicodeSpan (Str.matched_string s)))
    else None
end

module Hex = Make (struct
  let re =
    Str.regexp "&#x[0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F]?;"
end)

module Dec = Make (struct
  let re = Str.regexp "&#[0-9][0-9][0-9][0-9][0-9]?[0-9]?;"
end)
