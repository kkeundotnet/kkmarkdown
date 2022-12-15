let is_safe = false
let first_char = FirstChar.One '!'
let force_construct = true

type state = { alt : string; link : string; classes : string list }

let re = Str.regexp "!\\[\\(.*\\)\\](\\(.*\\))[ \t]*\\({\\(.*\\)}\\)?[ \t]*$"

let start line : state BlockRule.res =
  if Str.string_match re line 0 then
    let alt = Str.matched_group 1 line |> String.trim in
    let link = Str.matched_group 2 line |> String.trim in
    let classes =
      match Str.matched_group 4 line with
      | classes -> Utils.read_classes classes
      | exception Not_found -> []
    in
    Stop { state = { alt; link; classes }; handle_line = `Discard }
  else Die

let continue _ _ = assert false
let construct _ { alt; link; classes } _ = Typ.UnsafeImg { alt; link; classes }
