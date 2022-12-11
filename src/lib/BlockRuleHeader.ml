module Sharp = struct
  let is_safe = true
  let first_char = FirstChar.One '#'
  let force_construct = true

  type state = { level : int }

  let re = Str.regexp "\\(#+\\) "

  let start line : state BlockRule.res =
    if Str.string_match re line 0 then
      let level = String.length (Str.matched_group 1 line) in
      if level <= 6 then Stop { state = { level }; handle_line = `Keep }
      else Die
    else Die

  let continue state line = assert false

  let construct { BlockRule.trans_spans } { level } lines : Typ.block =
    let title =
      match lines with
      | [ line ] -> Str.string_after line (level + 1) |> trans_spans
      | _ -> assert false
    in
    match level with
    | 1 -> H1 title
    | 2 -> H2 title
    | 3 -> H3 title
    | 4 -> H4 title
    | 5 -> H5 title
    | 6 -> H6 title
    | _ -> assert false
end

module Make (M : sig
  val c : char
  val construct : Typ.span list -> Typ.block
end) =
struct
  let is_safe = true
  let first_char = FirstChar.Any
  let force_construct = false

  type state = unit

  let start _ = BlockRule.Go { state = (); handle_line = `Keep }
  let re = Str.regexp (String.make 3 M.c ^ "+[ \t]*$")

  let continue () line : state BlockRule.res =
    if Str.string_match re line 0 then
      Stop { state = (); handle_line = `Discard }
    else Die

  let construct { BlockRule.trans_spans } () lines =
    match lines with
    | [ title ] -> M.construct (trans_spans title)
    | _ -> assert false
end

module H1 = Make (struct
  let c = '='
  let construct spans = Typ.H1 spans
end)

module H2 = Make (struct
  let c = '-'
  let construct spans = Typ.H2 spans
end)
