module F = Format

type span =
  | NoneSpan
  | CharSpan of char
  | UnicodeSpan of string
  | Br
  | EmOpen
  | EmClose
  | StrongOpen
  | StrongClose
  | EmStrongOpen
  | EmStrongClose
  | CodeSpan of string list
  | A of string

type block =
  | P of span list
  | H1 of span list
  | H2 of span list
  | H3 of span list
  | H4 of span list
  | H5 of span list
  | H6 of span list
  | Quote of t
  | Ul of li list
  | Ol of li list
  | CodeBlock of string list
  | Hr

and li = Li of span list | LiP of block list

and t = block list

(* Pretty print *)

let pp_char =
  let special_chars =
    Char.Map.of_list
      [
        ('&', "&amp;");
        ('<', "&lt;");
        ('>', "&gt;");
        ('"', "&quot;");
        ('\'', "&apos;");
      ]
  in
  fun f c ->
    match Char.Map.find_opt c special_chars with
    | Some s -> F.pp_print_string f s
    | None -> F.pp_print_char f c

let pp_chars f s = String.iter (pp_char f) s

let pp_open f tag = F.fprintf f "<%s>" tag

let pp_close f tag = F.fprintf f "</%s>" tag

let pp_wrap tag pp f x =
  pp_open f tag;
  pp f x;
  pp_close f tag

let pp_list_with_line f = List.pp ~pp_sep:(fun f -> F.pp_print_char f '\n') f

let pp_span f = function
  | NoneSpan -> ()
  | CharSpan c -> pp_char f c
  | UnicodeSpan s -> F.pp_print_string f s
  | Br -> pp_open f "br"
  | EmOpen -> pp_open f "em"
  | EmClose -> pp_close f "em"
  | StrongOpen -> pp_open f "strong"
  | StrongClose -> pp_close f "strong"
  | EmStrongOpen ->
      pp_open f "em";
      pp_open f "strong"
  | EmStrongClose ->
      pp_close f "strong";
      pp_close f "em"
  | CodeSpan code -> pp_wrap "code" (pp_list_with_line pp_chars) f code
  | A s -> F.fprintf f {|<a href="%s">%a</a>|} s pp_chars s

let pp_span_list = List.pp pp_span

let rec pp_block f = function
  | P sps -> pp_wrap "p" pp_span_list f sps
  | Hr -> pp_open f "hr"
  | H1 sps -> pp_wrap "h1" pp_span_list f sps
  | H2 sps -> pp_wrap "h2" pp_span_list f sps
  | H3 sps -> pp_wrap "h3" pp_span_list f sps
  | H4 sps -> pp_wrap "h4" pp_span_list f sps
  | H5 sps -> pp_wrap "h5" pp_span_list f sps
  | H6 sps -> pp_wrap "h6" pp_span_list f sps
  | CodeBlock code_block ->
      pp_wrap "pre" (pp_wrap "code" (pp_list_with_line pp_chars)) f code_block
  | Quote quote -> pp_wrap "blockquote" pp f quote
  | Ol lis -> pp_wrap "ol" (pp_list_with_line pp_li) f lis
  | Ul lis -> pp_wrap "ul" (pp_list_with_line pp_li) f lis

and pp_li f = function
  | Li sps -> pp_wrap "li" pp_span_list f sps
  | LiP blocks -> pp_wrap "li" pp f blocks

and pp f = pp_list_with_line pp_block f

(* >>= *)

let gen_bind x f g = match f with Some _ as r -> r | None -> g x

(* Parse spans *)

type status = InEm | InStrong | InEmStrong

let status_equal x y = x = y

type spans_cont = { cur : int; status : status list; lines : string list }

let try_escape_char =
  let escape_chars =
    Char.Set.of_list
      [
        '\\';
        '`';
        '*';
        '_';
        '{';
        '}';
        '[';
        ']';
        '(';
        ')';
        '#';
        '+';
        '-';
        '.';
        '!';
      ]
  in
  fun ({ cur; lines; _ } as cont) ->
    match lines with
    | line :: _
      when cur + 1 < String.length line
           && Char.equal line.[cur] '\\'
           && Char.Set.mem line.[cur + 1] escape_chars ->
        Some (CharSpan line.[cur + 1], { cont with cur = cur + 2 })
    | _ -> None

let try_unicode =
  let get_unicode_size cur s =
    if
      (* &#xhhhh; *)
      cur + 7 < String.length s
      && Char.equal s.[cur] '&'
      && Char.equal s.[cur + 1] '#'
      && Char.equal s.[cur + 2] 'x'
      && Char.is_hexa s.[cur + 3]
      && Char.is_hexa s.[cur + 4]
      && Char.is_hexa s.[cur + 5]
      && Char.is_hexa s.[cur + 6]
      && Char.equal s.[cur + 7] ';'
    then Some 8
    else if
      (* &#xhhhhh; *)
      cur + 8 < String.length s
      && Char.equal s.[cur] '&'
      && Char.equal s.[cur + 1] '#'
      && Char.equal s.[cur + 2] 'x'
      && Char.is_hexa s.[cur + 3]
      && Char.is_hexa s.[cur + 4]
      && Char.is_hexa s.[cur + 5]
      && Char.is_hexa s.[cur + 6]
      && Char.is_hexa s.[cur + 7]
      && Char.equal s.[cur + 8] ';'
    then Some 9
    else if
      (* &#nnnn; *)
      cur + 6 < String.length s
      && Char.equal s.[cur] '&'
      && Char.equal s.[cur + 1] '#'
      && Char.is_num s.[cur + 2]
      && Char.is_num s.[cur + 3]
      && Char.is_num s.[cur + 4]
      && Char.is_num s.[cur + 5]
      && Char.equal s.[cur + 6] ';'
    then Some 7
    else None
  in
  fun ({ cur; lines; _ } as cont) ->
    match lines with
    | line :: _ -> (
        match get_unicode_size cur line with
        | None -> None
        | Some n ->
            Some
              (UnicodeSpan (String.sub line cur n), { cont with cur = cur + n })
        )
    | [] -> None

let try_paren s in_paren ~open_ ~close { cur; status; lines } =
  match lines with
  | line :: _ when String.is_sub cur line ~sub:s -> (
      let cur = cur + String.length s in
      match status with
      | hd :: tl when status_equal hd in_paren ->
          Some (close, { cur; status = tl; lines })
      | _ -> Some (open_, { cur; status = in_paren :: status; lines }) )
  | _ -> None

let try_em = try_paren "*" InEm ~open_:EmOpen ~close:EmClose

let try_strong = try_paren "**" InStrong ~open_:StrongOpen ~close:StrongClose

let try_em_strong =
  try_paren "***" InEmStrong ~open_:EmStrongOpen ~close:EmStrongClose

let try_code =
  let split_by_first_char c lines =
    let rec split lines rev =
      match lines with
      | [] -> None
      | line :: lines -> (
          match String.index_opt line c with
          | None -> split lines (line :: rev)
          | Some cur ->
              let code = String.sub line 0 cur in
              Some (List.rev (code :: rev), cur, line :: lines) )
    in
    split lines []
  in
  fun ({ cur; lines; _ } as cont) ->
    match lines with
    | line :: lines when cur < String.length line && Char.equal line.[cur] '`'
      -> (
        let lines = String.sub_from line (cur + 1) :: lines in
        match split_by_first_char '`' lines with
        | None -> Some (CodeSpan lines, { cont with cur = 0; lines = [] })
        | Some (code, cur, lines) ->
            Some (CodeSpan code, { cont with cur = cur + 1; lines }) )
    | _ -> None

let try_br ({ cur; lines; _ } as cont) =
  match lines with
  | line :: _
    when cur + 1 < String.length line
         && String.forall_from cur line ~f:(Char.equal ' ') ->
      Some (Br, { cont with cur = String.length line })
  | _ -> None

let try_char_span ({ cur; lines; _ } as cont) =
  match lines with
  | line :: lines' ->
      if cur < String.length line then
        Some (CharSpan line.[cur], { cont with cur = cur + 1 })
      else
        let r = match lines' with [] -> NoneSpan | _ :: _ -> CharSpan '\n' in
        Some (r, { cont with cur = 0; lines = lines' })
  | [] -> Some (NoneSpan, cont)

let try_a =
  let rec read_path_chars cur line =
    if cur < String.length line then
      let c = line.[cur] in
      if Char.equal c '>' then Some cur
      else if Char.equal c '"' || Char.equal c '\'' then None
      else read_path_chars (cur + 1) line
    else None
  in
  let read_path cur line =
    if String.is_sub cur line ~sub:"https://" then
      read_path_chars (cur + 8) line
    else if String.is_sub cur line ~sub:"http://" then
      read_path_chars (cur + 7) line
    else None
  in
  fun ({ cur; lines; _ } as cont) ->
    match lines with
    | line :: _ when cur < String.length line && Char.equal line.[cur] '<' -> (
        let start = cur + 1 in
        match read_path start line with
        | Some end_ ->
            let link = String.sub line start (end_ - start) in
            Some (A link, { cont with cur = end_ + 1 })
        | None -> None )
    | _ -> None

let trans_spans =
  let rec close_status rev = function
    | [] -> rev
    | InEm :: status -> close_status (EmClose :: rev) status
    | InStrong :: status -> close_status (StrongClose :: rev) status
    | InEmStrong :: status -> close_status (EmStrongClose :: rev) status
  in
  let rec trans ({ status; lines; _ } as cont) rev =
    match lines with
    | [] -> close_status rev status |> List.rev
    | _ :: _ ->
        let ( >>= ) = gen_bind cont in
        None >>= try_escape_char >>= try_unicode >>= try_em_strong
        >>= try_strong >>= try_em >>= try_code >>= try_br >>= try_a
        >>= try_char_span |> Option.value_exn
        |> fun (span, cont) -> (trans [@tailcall]) cont (span :: rev)
  in
  fun lines -> trans { cur = 0; status = []; lines } []

let trans_spans_of_line line = trans_spans [ line ]

(* Parse blocks *)

let is_empty_line line =
  String.forall line ~f:(fun c -> Char.equal ' ' c || Char.equal '\t' c)

let is_ul_indent_start line = String.is_prefix line ~prefix:"* "

let is_ol_indent_start_3 line =
  String.length line >= 3
  && Char.is_num line.[0]
  && Char.equal line.[1] '.'
  && Char.equal line.[2] ' '

let is_ol_indent_start_4 line =
  String.length line >= 4
  && Char.is_num line.[0]
  && Char.is_num line.[1]
  && Char.equal line.[2] '.'
  && Char.equal line.[3] ' '

let is_ol_indent_start line =
  is_ol_indent_start_3 line || is_ol_indent_start_4 line

let remove_ul_indent line =
  if
    String.is_prefix line ~prefix:"*   " || String.is_prefix line ~prefix:"    "
  then String.sub_from line 4
  else if
    String.is_prefix line ~prefix:"*  " || String.is_prefix line ~prefix:"   "
  then String.sub_from line 3
  else if
    String.is_prefix line ~prefix:"* " || String.is_prefix line ~prefix:"  "
  then String.sub_from line 2
  else if String.is_prefix line ~prefix:" " then String.sub_from line 1
  else line

let remove_ol_indent line =
  if
    is_ol_indent_start_4 line
    || String.length line >= 4
       && is_ol_indent_start_3 line
       && Char.equal line.[3] ' '
    || String.is_prefix line ~prefix:"    "
  then String.sub_from line 4
  else if is_ol_indent_start_3 line || String.is_prefix line ~prefix:"   " then
    String.sub_from line 3
  else if String.is_prefix line ~prefix:"  " then String.sub_from line 2
  else if String.is_prefix line ~prefix:" " then String.sub_from line 1
  else line

let try_hr =
  let is_hr line =
    String.length line >= 3 && String.forall line ~f:(Char.equal '*')
  in
  function line :: lines when is_hr line -> Some (Hr, lines) | _ -> None

let try_header_by_sharp =
  let try_header_line line =
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
      try_header_line line |> Option.map (fun header -> (header, lines))
  | [] -> None

let try_header_by_dash = function
  | line1 :: line2 :: lines
    when String.length line2 >= 3 && String.forall line2 ~f:(Char.equal '=') ->
      Some (H1 (trans_spans_of_line line1), lines)
  | line1 :: line2 :: lines
    when String.length line2 >= 3 && String.forall line2 ~f:(Char.equal '-') ->
      Some (H2 (trans_spans_of_line line1), lines)
  | _ -> None

let try_header lines =
  let ( >>= ) = gen_bind lines in
  None >>= try_header_by_sharp >>= try_header_by_dash

let try_code_block_by_bound =
  let is_code_block_bound line =
    String.length line >= 3
    && ( String.forall line ~f:(Char.equal '`')
       || String.forall line ~f:(Char.equal '~') )
  in
  function
  | line :: lines when is_code_block_bound line -> (
      match List.split_by_first lines ~f:(String.equal line) with
      | None -> Some (CodeBlock lines, [])
      | Some (cb, _, lines) -> Some (CodeBlock cb, lines) )
  | _ -> None

let try_code_block_by_indent =
  let is_code_block_indent line = String.is_prefix line ~prefix:"    " in
  let remove_indent lines = List.map (fun s -> String.sub_from s 4) lines in
  function
  | line :: lines as x when is_code_block_indent line -> (
      match
        List.split_by_first lines ~f:(fun line ->
            not (is_code_block_indent line))
      with
      | None -> Some (CodeBlock (remove_indent x), [])
      | Some (cb, line, lines) ->
          Some (CodeBlock (remove_indent cb), line :: lines) )
  | _ -> None

let try_code_block lines =
  let ( >>= ) = gen_bind lines in
  None >>= try_code_block_by_bound >>= try_code_block_by_indent

let try_p lines =
  let p, lines =
    match List.split_by_first lines ~f:is_empty_line with
    | None -> (lines, [])
    | Some (p, _, lines) -> (p, lines)
  in
  Some (P (trans_spans p), lines)

let rec gen_try_xl constructor is_indent_start remove_indent =
  let trans_xl_elems lines =
    let groups =
      List.strip lines ~f:is_empty_line |> List.group ~f:is_indent_start
    in
    let trans_elem =
      if List.exists (List.exists is_empty_line) groups then fun lines ->
        LiP (trans_from_lines lines)
      else fun lines -> Li (trans_spans lines)
    in
    List.map (fun group -> List.map remove_indent group |> trans_elem) groups
  in
  let is_xl_indent line =
    is_indent_start line
    || String.is_prefix line ~prefix:" "
    || is_empty_line line
  in
  fun lines ->
    match lines with
    | line :: lines' when is_indent_start line -> (
        match
          List.split_by_first lines' ~f:(fun line -> not (is_xl_indent line))
        with
        | None -> Some (constructor (trans_xl_elems lines), [])
        | Some (xl, cont_line, cont_lines) ->
            Some
              ( constructor (trans_xl_elems (line :: xl)),
                cont_line :: cont_lines ) )
    | _ -> None

and try_ul lines =
  gen_try_xl (fun x -> Ul x) is_ul_indent_start remove_ul_indent lines

and try_ol lines =
  gen_try_xl (fun x -> Ol x) is_ol_indent_start remove_ol_indent lines

and try_quote =
  let is_quote_indent line = String.is_prefix line ~prefix:"> " in
  let remove_indent lines =
    let remove_indent line =
      if
        String.is_prefix line ~prefix:"> " || String.is_prefix line ~prefix:"  "
      then String.sub_from line 2
      else if
        String.is_prefix line ~prefix:">" || String.is_prefix line ~prefix:" "
      then String.sub_from line 1
      else line
    in
    List.map remove_indent lines
  in
  fun lines ->
    match lines with
    | line :: lines' when is_quote_indent line -> (
        match List.split_by_first lines' ~f:is_empty_line with
        | None -> Some (Quote (remove_indent lines |> trans_from_lines), [])
        | Some (quote, _, lines) ->
            Some
              (Quote (remove_indent (line :: quote) |> trans_from_lines), lines)
        )
    | _ -> None

and trans_from_lines lines =
  let rec trans lines rev =
    match List.remove_head lines ~f:is_empty_line with
    | [] -> List.rev rev
    | lines ->
        let ( >>= ) = gen_bind lines in
        None >>= try_hr >>= try_code_block >>= try_header >>= try_quote
        >>= try_ul >>= try_ol >>= try_p |> Option.value_exn
        |> fun (r, lines) -> (trans [@tailcall]) lines (r :: rev)
  in
  trans lines []

let trans s = String.split_to_lines s |> trans_from_lines

let trans_to_string s = trans s |> F.asprintf "%a" pp
