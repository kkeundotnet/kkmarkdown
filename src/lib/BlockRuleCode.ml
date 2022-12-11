module Make (M : sig
  val is_safe : bool
  val c : char
end) =
struct
  let is_safe = M.is_safe
  let first_char = FirstChar.One M.c
  let force_construct = true

  type state = { len : int; classes : string list }

  let prefix = Format.sprintf "\\(%c%c%c+\\)" M.c M.c M.c
  let classes = "[ \t]*{\\(.*\\)}"
  let postfix = "[ \t]*$"
  let re_with_classes = Str.regexp (prefix ^ classes ^ postfix)
  let re_without_classes = Str.regexp (prefix ^ postfix)

  let start line : state BlockRule.res =
    let re = if is_safe then re_without_classes else re_with_classes in
    if Str.string_match re line 0 then
      let len = String.length (Str.matched_group 1 line) in
      let classes =
        if is_safe then [] else Str.matched_group 2 line |> Utils.read_classes
      in
      Go { state = { len; classes }; handle_line = `Discard }
    else Die

  let continue ({ len; classes } as state) line : state BlockRule.res =
    if Str.string_match re_without_classes line 0 then
      if len = String.length (Str.matched_group 1 line) then
        Stop { state; handle_line = `Discard }
      else Go { state; handle_line = `Keep }
    else Go { state; handle_line = `Keep }

  let construct _ { classes } lines =
    if is_safe then Typ.CodeBlock lines
    else Typ.UnsafeCodeBlock { cb = lines; classes }
end

module Backquote = Make (struct
  let is_safe = true
  let c = '`'
end)

module Tilde = Make (struct
  let is_safe = true
  let c = '~'
end)

module UnsafeBackquote = Make (struct
  let is_safe = false
  let c = '`'
end)

module UnsafeTilde = Make (struct
  let is_safe = false
  let c = '~'
end)

module Indent = struct
  let is_safe = true
  let first_char = FirstChar.One ' '
  let force_construct = true

  type state = unit

  let re = Str.regexp "    "

  let start line : state BlockRule.res =
    if Str.string_match re line 0 then Go { state = (); handle_line = `Keep }
    else Die

  let continue () line : state BlockRule.res =
    if Str.string_match re line 0 then Go { state = (); handle_line = `Keep }
    else Stop { state = (); handle_line = `Left }

  let construct _ () lines =
    let lines = List.map (fun line -> Str.string_after line 4) lines in
    Typ.CodeBlock lines
end
