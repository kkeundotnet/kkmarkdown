(* TODO rename *)
module F = Format

(* TODO
block elements
- paragraph
- [done] headers
- block quote
- lists
- [done] code blocks
- [done] horizontal rules

span elements
- line breaks
- emphasis
- code

later
- restricted links
 *)

type span =
  | NoneSpan
  | VChar of char
  | VSpace
  | Br
  | Em of span
  | Strong of span
  | CodeSpan

type block =
  | P of span list
  | H1 of span list
  | H2 of span list
  | H3 of span list
  | H4 of span list
  | H5 of span list
  | H6 of span list
  | Quote
  | Lists
  | CodeBlock of string list
  | Hr

type t = block list

module CharSet = Set.Make (Char)

let escape_chars =
  CharSet.of_list
    ['\\'; '`'; '*'; '_'; '{'; '}'; '['; ']'; '('; ')'; '#'; '+'; '-'; '.'; '!']

module CharMap = Map.Make (Char)

let special_chars =
  CharMap.of_list
    [ ('&', "&amp;")
    ; ('<', "&lt;")
    ; ('>', "&gt;")
    ; ('"', "&quot;")
    ; ('\'', "&apos;") ]

let pp_char f c =
  match CharMap.find_opt c special_chars with
  | Some s ->
      F.pp_print_string f s
  | None ->
      F.pp_print_char f c

let pp_unicode_format f s n =
  if
    (* &#xhhhh; *)
    n + 7 < String.length s
    && Char.equal s.[n] '&'
    && Char.equal s.[n + 1] '#'
    && Char.equal s.[n + 2] 'x'
    && Char.is_hexa s.[n + 3]
    && Char.is_hexa s.[n + 4]
    && Char.is_hexa s.[n + 5]
    && Char.is_hexa s.[n + 6]
    && Char.equal s.[n + 7] ';'
  then (
    let sub = String.sub s n 8 in
    F.pp_print_string f sub ; Some 8 )
  else if
    (* &#nnnn; *)
    n + 6 < String.length s
    && Char.equal s.[n] '&'
    && Char.equal s.[n + 1] '#'
    && Char.is_num s.[n + 2]
    && Char.is_num s.[n + 3]
    && Char.is_num s.[n + 4]
    && Char.is_num s.[n + 5]
    && Char.equal s.[n + 6] ';'
  then (
    let sub = String.sub s n 7 in
    F.pp_print_string f sub ; Some 7 )
  else None

let pp_escape ~unicode f s =
  let rec pp_unicode_escape f s n =
    let pp () =
      pp_char f s.[n] ;
      (pp_unicode_escape [@tailcall]) f s (n + 1)
    in
    if n < String.length s then
      if unicode then
        match pp_unicode_format f s n with
        | None ->
            pp ()
        | Some m ->
            pp_unicode_escape f s (n + m)
      else pp ()
  in
  pp_unicode_escape f s 0

let pp_wrap tag pp f x = F.fprintf f "<%s>%a</%s>" tag pp x tag

let pp_list pp f l = List.iter (pp f) l

let rec pp_span f = function
  | NoneSpan ->
      ()
  | VChar c ->
      pp_char f c
  | VSpace ->
      F.pp_print_char f ' '
  | Br ->
      F.pp_print_string f "<br>"
  | Em sp ->
      pp_wrap "em" pp_span f sp
  | Strong sp ->
      pp_wrap "strong" pp_span f sp
  | CodeSpan ->
      assert false

let pp_span_list = pp_list pp_span

let pp_block f = function
  | P sps ->
      pp_wrap "p" pp_span_list f sps
  | Hr ->
      F.pp_print_string f "<hr>"
  | H1 sps ->
      pp_wrap "h1" pp_span_list f sps
  | H2 sps ->
      pp_wrap "h2" pp_span_list f sps
  | H3 sps ->
      pp_wrap "h3" pp_span_list f sps
  | H4 sps ->
      pp_wrap "h4" pp_span_list f sps
  | H5 sps ->
      pp_wrap "h5" pp_span_list f sps
  | H6 sps ->
      pp_wrap "h6" pp_span_list f sps
  | CodeBlock code_block ->
      pp_wrap "pre"
        (pp_wrap "code"
           (pp_list (fun f -> F.fprintf f "%a@\n" (pp_escape ~unicode:false))))
        f code_block
  | _ ->
      assert false

let pp = pp_list pp_block

(* Parsing *)

let gen_bind x f g = match f with Some _ as r -> r | None -> g x

let gen_bind2 x y f g = match f with Some _ as r -> r | None -> g x y

let is_hr line =
  String.length line >= 3 && String.forall line ~f:(Char.equal '*')

let is_code_block_bound line =
  String.length line >= 3
  && ( String.forall line ~f:(Char.equal '`')
     || String.forall line ~f:(Char.equal '~') )

let is_code_block_indent line = String.is_prefix line ~prefix:"    "

let is_empty_line line =
  String.forall line ~f:(fun c -> Char.equal ' ' c || Char.equal '\t' c)

let remove_indent line = List.map (fun s -> String.sub_from s 4) line

let rec try_escape_char cur lines =
  match lines with
  | line :: _
    when cur + 1 < String.length line
         && Char.equal line.[cur] '\\'
         && CharSet.mem line.[cur + 1] escape_chars ->
      Some (VChar line.[cur + 1], cur + 2, lines)
  | _ ->
      None

let rec try_v_char cur lines =
  match lines with
  | [] ->
      assert false
  | line :: lines' -> (
      if cur < String.length line then Some (VChar line.[cur], cur + 1, lines)
      else
        match lines' with
        | [] ->
            Some (NoneSpan, 0, lines')
        | _ :: _ ->
            Some (VSpace, 0, lines') )

let trans_spans lines =
  let rec trans cur lines rev =
    match lines with
    | [] ->
        List.rev rev
    | _ :: _ -> (
        let ( >>= ) = gen_bind2 cur lines in
        None >>= try_escape_char >>= try_v_char
        |> function
        | None ->
            assert false
        | Some (span, cur, lines) ->
            (trans [@tailcall]) cur lines (span :: rev) )
  in
  trans 0 lines []

let trans_spans_of_line line = trans_spans [line]

let try_parse_hr = function
  | line :: lines when is_hr line ->
      Some (Hr, lines)
  | _ ->
      None

let try_parse_header_by_sharp =
  let try_parse_header_line line =
    if String.is_prefix line ~prefix:"# " then
      Some (H1 (trans_spans_of_line (String.sub_from line 2)))
    else if String.is_prefix line ~prefix:"## " then
      Some (H2 (trans_spans_of_line (String.sub_from line 3)))
    else if String.is_prefix line ~prefix:"### " then
      Some (H3 (trans_spans_of_line (String.sub_from line 4)))
    else if String.is_prefix line ~prefix:"#### " then
      Some (H4 (trans_spans_of_line (String.sub_from line 5)))
    else if String.is_prefix line ~prefix:"##### " then
      Some (H5 (trans_spans_of_line (String.sub_from line 6)))
    else if String.is_prefix line ~prefix:"###### " then
      Some (H6 (trans_spans_of_line (String.sub_from line 7)))
    else None
  in
  function
  | line :: lines ->
      try_parse_header_line line |> Option.map (fun header -> (header, lines))
  | [] ->
      None

let try_parse_header_by_dash = function
  | line1 :: line2 :: lines
    when String.length line2 >= 3 && String.forall line2 ~f:(Char.equal '=') ->
      Some (H1 (trans_spans_of_line line1), lines)
  | line1 :: line2 :: lines
    when String.length line2 >= 3 && String.forall line2 ~f:(Char.equal '-') ->
      Some (H2 (trans_spans_of_line line1), lines)
  | _ ->
      None

let try_parse_header lines =
  let ( >>= ) = gen_bind lines in
  None >>= try_parse_header_by_sharp >>= try_parse_header_by_dash

let try_parse_code_block_by_bound = function
  | line :: lines when is_code_block_bound line -> (
    match List.split_by_first lines ~f:(String.equal line) with
    | None ->
        Some (CodeBlock lines, [])
    | Some (cb, _, lines) ->
        Some (CodeBlock cb, lines) )
  | _ ->
      None

let try_parse_code_block_by_indent x =
  match x with
  | line :: lines when is_code_block_indent line -> (
    match
      List.split_by_first lines ~f:(fun line ->
          not (is_code_block_indent line))
    with
    | None ->
        Some (CodeBlock (remove_indent x), [])
    | Some (cb, line, lines) ->
        Some (CodeBlock (remove_indent cb), line :: lines) )
  | _ ->
      None

let try_parse_code_block lines =
  let ( >>= ) = gen_bind lines in
  None >>= try_parse_code_block_by_bound >>= try_parse_code_block_by_indent

let try_parse_p lines =
  let p, lines =
    match List.split_by_first lines ~f:is_empty_line with
    | None ->
        (lines, [])
    | Some (p, _, lines) ->
        (p, lines)
  in
  Some (P (trans_spans p), lines)

let trans_from_string_list lines =
  let rec trans lines rev =
    match List.remove_head lines ~f:is_empty_line with
    | [] ->
        List.rev rev
    | _ -> (
        let ( >>= ) = gen_bind lines in
        None >>= try_parse_hr >>= try_parse_code_block >>= try_parse_header
        >>= try_parse_p
        |> function
        | Some (r, lines) ->
            (trans [@tailcall]) lines (r :: rev)
        | None ->
            (* TODO: exit 1 *)
            rev )
  in
  trans lines []

let trans s = trans_from_string_list (String.split_on_char '\n' s)
