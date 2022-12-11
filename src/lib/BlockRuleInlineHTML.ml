module Make (M : sig
  val tag : string
end) =
struct
  let is_safe = false
  let first_char = FirstChar.One '<'
  let force_construct = false

  type state = unit

  let re_start = Str.regexp ("<[ \t]*" ^ M.tag ^ "[ \t>]")
  let re_end = Str.regexp ("</[ \t]*" ^ M.tag ^ "[ \t]*>[ \t]*$")

  let start line : state BlockRule.res =
    if Str.string_match re_start line 0 then
      Go { state = (); handle_line = `Keep }
    else Die

  let continue _ line : state BlockRule.res =
    if Str.string_match re_end line 0 then
      Stop { state = (); handle_line = `Keep }
    else Go { state = (); handle_line = `Keep }

  let construct _ _ lines = Typ.UnsafeInlineHtml lines
end

module Div = Make (struct
  let tag = "div"
end)

module Script = Make (struct
  let tag = "script"
end)
