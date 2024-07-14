let is_safe = true
let first_char = FirstChar.One '|'

type reading = Title | Body
type state = { reading : reading; title_lines : int }

let re_start = Str.regexp "|>  ?"
let re_end = Str.regexp "<|[ \t]*$"

let start line : state BlockRule.res =
  if Str.string_match re_start line 0 then
    Go { state = { reading = Title; title_lines = 1 }; handle_line = `Keep }
  else Die

let continue ({ reading; title_lines } as state) line : state BlockRule.res =
  match reading with
  | Title ->
      if Utils.is_empty_line line then
        Go { state = { state with reading = Body }; handle_line = `Discard }
      else
        Go
          {
            state = { state with title_lines = title_lines + 1 };
            handle_line = `Keep;
          }
  | Body ->
      if Str.string_match re_end line 0 then
        Stop { state; handle_line = `Discard }
      else Go { state; handle_line = `Keep }

let take_n n lines =
  let rec aux n (rev, lines) =
    match lines with
    | line :: lines when n > 0 -> aux (n - 1) (line :: rev, lines)
    | _ -> (List.rev rev, lines)
  in
  aux n ([], lines)

let remove_title_indent line =
  if
    Str.string_match re_start line 0
    || Str.string_match Utils.re_spaces_upto_four line 0
  then Str.string_after line (Str.match_end ())
  else line

let construct { BlockRule.trans_spans_from_lines; BlockRule.trans_blocks }
    { title_lines } lines : Typ.block =
  let title_lines, body_lines = take_n title_lines lines in
  let title =
    trans_spans_from_lines (List.map remove_title_indent title_lines)
  in
  let body = trans_blocks body_lines in
  Details { title; body }

let force_construct = true
