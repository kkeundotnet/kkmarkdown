module Make (M : sig
  val first_char : FirstChar.t
  val re : Str.regexp
  val list_typ : Typ.list_typ
end) : BlockRule.S = struct
  let is_safe = true
  let first_char = M.first_char
  let force_construct = true

  type state = { was_empty_line : bool }

  let start line : state BlockRule.res =
    if Str.string_match M.re line 0 then
      Go { state = { was_empty_line = false }; handle_line = `Keep }
    else Die

  let match_indent line =
    Str.string_match M.re line 0
    || Str.string_match Utils.re_spaces_upto_four line 0

  let continue ({ was_empty_line } as state) line : state BlockRule.res =
    if match_indent line then
      Go { state = { was_empty_line = false }; handle_line = `Keep }
    else if was_empty_line then Stop { state; handle_line = `Left }
    else
      Go
        {
          state = { was_empty_line = Utils.is_empty_line line };
          handle_line = `Keep;
        }

  let remove_trailing_empty_line lines =
    match List.rev lines with
    | hd :: tl when Utils.is_empty_line hd -> List.rev tl
    | _ -> lines

  let remove_trailing_empty_line_rev rev_lines =
    match rev_lines with
    | hd :: tl when Utils.is_empty_line hd -> tl
    | _ -> rev_lines

  let separate_lis lines =
    List.fold_left
      (fun acc line ->
        if Str.string_match M.re line 0 then [ line ] :: acc
        else
          match acc with hd :: tl -> (line :: hd) :: tl | [] -> assert false)
      [] lines

  let remove_indent line =
    if match_indent line then Str.string_after line (Str.match_end ()) else line

  let construct { BlockRule.trans_spans_from_lines; trans_blocks } _ lines =
    let lines = remove_trailing_empty_line lines in
    let has_empty_line = List.exists Utils.is_empty_line lines in
    let lis =
      separate_lis lines
      |> List.rev_map (fun rev_lines ->
             remove_trailing_empty_line_rev rev_lines
             |> List.rev_map remove_indent)
    in
    let construct_li : string list -> Typ.li =
      if has_empty_line then fun lines -> LiP (trans_blocks lines)
      else fun lines -> Li (trans_spans_from_lines lines)
    in
    Typ.List_ (M.list_typ, List.map construct_li lis)
end

module Ol = Make (struct
  let first_char =
    FirstChar.OneOf [ '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9' ]

  let re = Str.regexp "[0-9]\\(\\.  ?\\|[0-9]\\. \\)"
  let list_typ = Typ.Ordered
end)

module UlMake (M : sig
  val char : char
end) =
Make (struct
  let first_char = FirstChar.One M.char
  let re = Str.regexp (String.make 1 M.char ^ " \\(  \\| \\|\\)")
  let list_typ = Typ.Unordered
end)

module UlStar = UlMake (struct
  let char = '*'
end)

module UlPlus = UlMake (struct
  let char = '+'
end)

module UlMinus = UlMake (struct
  let char = '-'
end)
