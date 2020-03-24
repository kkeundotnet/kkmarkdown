module F = Format

type span =
  | NoneSpan
  | CharSpan of char
  | CharsSpan of string
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
  | UnsafeA of { spans : span list; link : string }

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
  | UnsafeCodeBlock of { cb : string list; classes : string list }
  | UnsafeImg of { alt : string; link : string; classes : string list }
  | UnsafeInlineHtml of string list

and li = Li of span list | LiP of block list

and t = block list

(* Pretty print *)

let pp_char f = function
  | '&' -> F.pp_print_string f "&amp;"
  | '<' -> F.pp_print_string f "&lt;"
  | '>' -> F.pp_print_string f "&gt;"
  | '"' -> F.pp_print_string f "&quot;"
  | '\'' -> F.pp_print_string f "&apos;"
  | c -> F.pp_print_char f c

let pp_chars f s = String.iter (pp_char f) s

let pp_classes f classes =
  List.pp ~pp_sep:(fun f -> F.pp_print_char f ' ') F.pp_print_string f classes

let pp_open ?classes f tag =
  match classes with
  | None -> F.fprintf f {|<%s>|} tag
  | Some classes -> F.fprintf f {|<%s class="%a">|} tag pp_classes classes

let pp_close f tag = F.fprintf f "</%s>" tag

let pp_wrap tag ?classes pp f x =
  ( match classes with
  | None -> pp_open f tag
  | Some classes -> pp_open ~classes f tag );
  pp f x;
  pp_close f tag

let pp_list_with_line f = List.pp ~pp_sep:(fun f -> F.pp_print_char f '\n') f

let rec pp_span f = function
  | NoneSpan -> ()
  | CharSpan c -> pp_char f c
  | CharsSpan s -> pp_chars f s
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
  | UnsafeA { spans; link } ->
      F.fprintf f {|<a href="%s">%a</a>|} link pp_span_list spans

and pp_span_list f = List.pp pp_span f

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
  | UnsafeCodeBlock { cb; classes } ->
      pp_wrap "pre" ~classes (pp_wrap "code" (pp_list_with_line pp_chars)) f cb
  | UnsafeInlineHtml lines ->
      List.pp
        ~pp_sep:(fun f -> F.pp_print_newline f ())
        F.pp_print_string f lines
  | UnsafeImg { alt; link; classes } ->
      let pp_img f () =
        F.fprintf f {|<img alt="%s" src="%s" class="%a">|} alt link pp_classes
          classes
      in
      pp_wrap "p" pp_img f ()

and pp_li f = function
  | Li sps -> pp_wrap "li" pp_span_list f sps
  | LiP blocks -> pp_wrap "li" pp f blocks

and pp f = pp_list_with_line pp_block f

(* >>= *)

let gen_bind x f g = match f with Some _ as r -> r | None -> g x

(* Continuation status *)

type status = InEm | InStrong | InEmStrong

let status_equal x y = x = y

type spans_cont = { cur : int; status : status list; lines : string list }

(* Parse unsafe spans/blocks *)

module Unsafe : sig
  type unsafe_f_span

  type unsafe_f_block

  val try_span :
    unsafe:bool -> unsafe_f_span -> spans_cont -> (span * spans_cont) option

  val try_block :
    unsafe:bool -> unsafe_f_block -> string list -> (block * string list) option

  val a : trans_spans_of_line:(string -> span list) -> unsafe_f_span

  val img : unsafe_f_block

  val code_block : unsafe_f_block

  val div : unsafe_f_block

  val script : unsafe_f_block
end = struct
  let ( let* ) = Option.bind

  let ( let+ ) x f = Option.map f x

  type unsafe_f_span = spans_cont -> (span * spans_cont) option

  type unsafe_f_block = string list -> (block * string list) option

  let try_ ~unsafe f x = if unsafe then f x else None

  let try_span = try_

  let try_block = try_

  let read_classes s =
    String.split_on_char ' ' s
    |> List.filter_map (fun class_ ->
           if String.length class_ >= 2 && Char.equal class_.[0] '.' then
             Some (String.sub_from class_ 1)
           else None)

  (* ![alt](link) {.class1 .class2} *)
  let img = function
    | line :: lines
      when String.length line >= 7
           && Char.equal line.[0] '!'
           && Char.equal line.[1] '['
           && Char.equal line.[String.length line - 1] '}' ->
        let* cur1 = String.index_sub_opt line ~sub:"](" in
        let* cur2 = String.index_sub_opt line ~sub:") {" in
        if cur1 < cur2 then
          let alt = String.sub line 2 (cur1 - 2) |> String.trim in
          let link =
            String.sub line (cur1 + 2) (cur2 - (cur1 + 2)) |> String.trim
          in
          let cur = cur2 + 3 in
          let classes =
            String.sub line cur (String.length line - cur - 1) |> read_classes
          in
          Some (UnsafeImg { alt; link; classes }, lines)
        else None
    | _ -> None

  (* ``` {.class1 .class2} *)
  let code_block = function
    | line :: lines
      when String.length line >= 6
           && Char.equal line.[0] '`'
           && Char.equal line.[1] '`'
           && Char.equal line.[2] '`'
           && Char.equal line.[String.length line - 1] '}' -> (
        let+ cur = String.index_sub_opt line ~sub:"` {" in
        let code_block_bound = String.sub line 0 (cur + 1) in
        let cur = cur + 3 in
        let classes =
          String.sub line cur (String.length line - cur - 1) |> read_classes
        in
        match List.split_by_first lines ~f:(String.equal code_block_bound) with
        | None -> (UnsafeCodeBlock { cb = lines; classes }, [])
        | Some (cb, _, lines) -> (UnsafeCodeBlock { cb; classes }, lines) )
    | _ -> None

  let gen_inline_html ~tag =
    let tag_len = String.length tag in
    function
    | line :: lines as all
      when String.is_prefix line ~prefix:("<" ^ tag)
           && 1 + tag_len < String.length line
           && ( Char.equal line.[1 + tag_len] ' '
              || Char.equal line.[1 + tag_len] '>' ) ->
        Some
          ( match
              List.split_by_first lines ~f:(String.equal ("</" ^ tag ^ ">"))
            with
          | None -> (UnsafeInlineHtml all, [])
          | Some (div_body, div_close, lines) ->
              ( UnsafeInlineHtml
                  (line :: List.append_tailrec div_body [ div_close ]),
                lines ) )
    | _ -> None

  (* <div> *)
  let div = gen_inline_html ~tag:"div"

  (* <script> *)
  let script = gen_inline_html ~tag:"script"

  (* [text](link) *)
  let a ~trans_spans_of_line { cur; status; lines } =
    match lines with
    | line :: _ when cur + 3 < String.length line && Char.equal line.[cur] '['
      ->
        let* cur1 = String.index_sub_from_opt (cur + 1) line ~sub:"](" in
        let* cur2 = String.index_from_opt line (cur1 + 2) ')' in
        let text = String.sub line (cur + 1) (cur1 - (cur + 1)) in
        let link = String.sub line (cur1 + 2) (cur2 - (cur1 + 2)) in
        Some
          ( UnsafeA { spans = trans_spans_of_line text; link },
            { cur = cur2 + 1; status; lines } )
    | _ -> None
end

(* Parse spans *)

let try_escape_char =
  let is_escape_char = function
    | '\\' | '`' | '*' | '_' | '{' | '}' | '[' | ']' | '(' | ')' | '#' | '+'
    | '-' | '.' | '!' ->
        true
    | _ -> false
  in
  fun ({ cur; lines; _ } as cont) ->
    match lines with
    | line :: _
      when cur + 1 < String.length line
           && Char.equal line.[cur] '\\'
           && is_escape_char line.[cur + 1] ->
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
        let len = String.length line in
        let rec read_chars n =
          if n < len then
            match line.[n] with
            | '[' | '\\' | '&' | '*' | '`' | ' ' | '<' -> n
            | _ -> read_chars (n + 1)
          else n
        in
        let cur' = read_chars (cur + 1) in
        Some
          ( CharsSpan (String.sub line cur (cur' - cur)),
            { cont with cur = cur' } )
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

let rec trans_spans ~unsafe =
  let rec close_status rev = function
    | [] -> rev
    | InEm :: status -> close_status (EmClose :: rev) status
    | InStrong :: status -> close_status (StrongClose :: rev) status
    | InEmStrong :: status -> close_status (EmStrongClose :: rev) status
  in
  let rec trans ({ status; lines; cur } as cont) rev =
    match lines with
    | [] -> close_status rev status |> List.rev
    | line :: _ ->
        let ( >>= ) = gen_bind cont in
        ( if cur < String.length line then
          None
          >>=
          match line.[cur] with
          | '[' ->
              Unsafe.(
                try_span ~unsafe
                  (a ~trans_spans_of_line:(trans_spans_of_line ~unsafe)))
          | '\\' -> try_escape_char
          | '&' -> try_unicode
          | '*' -> fun _cont -> None >>= try_em_strong >>= try_strong >>= try_em
          | '`' -> try_code
          | ' ' -> try_br
          | '<' -> try_a
          | _ -> fun _cont -> None
        else None )
        >>= try_char_span |> Option.value_exn
        |> fun (span, cont) -> (trans [@tailcall]) cont (span :: rev)
  in
  fun lines -> trans { cur = 0; status = []; lines } []

and trans_spans_of_line ~unsafe line = trans_spans ~unsafe [ line ]

(* Parse blocks *)

let is_empty_line line =
  String.forall line ~f:(function ' ' | '\t' -> true | _ -> false)

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
  let try_header_line ~unsafe line =
    if String.is_prefix line ~prefix:"# " then
      Some (H1 (trans_spans_of_line ~unsafe (String.sub_from line 2)))
    else if String.is_prefix line ~prefix:"## " then
      Some (H2 (trans_spans_of_line ~unsafe (String.sub_from line 3)))
    else if String.is_prefix line ~prefix:"### " then
      Some (H3 (trans_spans_of_line ~unsafe (String.sub_from line 4)))
    else if String.is_prefix line ~prefix:"#### " then
      Some (H4 (trans_spans_of_line ~unsafe (String.sub_from line 5)))
    else if String.is_prefix line ~prefix:"##### " then
      Some (H5 (trans_spans_of_line ~unsafe (String.sub_from line 6)))
    else if String.is_prefix line ~prefix:"###### " then
      Some (H6 (trans_spans_of_line ~unsafe (String.sub_from line 7)))
    else None
  in
  fun ~unsafe -> function
    | line :: lines ->
        try_header_line ~unsafe line
        |> Option.map (fun header -> (header, lines))
    | [] -> None

let try_header_by_dash ~unsafe = function
  | line1 :: line2 :: lines
    when String.length line2 >= 3 && String.forall line2 ~f:(Char.equal '=') ->
      Some (H1 (trans_spans_of_line ~unsafe line1), lines)
  | line1 :: line2 :: lines
    when String.length line2 >= 3 && String.forall line2 ~f:(Char.equal '-') ->
      Some (H2 (trans_spans_of_line ~unsafe line1), lines)
  | _ -> None

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

let try_p ~unsafe lines =
  let p, lines =
    match List.split_by_first lines ~f:is_empty_line with
    | None -> (lines, [])
    | Some (p, _, lines) -> (p, lines)
  in
  Some (P (trans_spans ~unsafe p), lines)

let rec gen_try_xl constructor is_indent_start remove_indent =
  let trans_xl_elems ~unsafe lines =
    let groups =
      List.strip lines ~f:is_empty_line |> List.group ~f:is_indent_start
    in
    let trans_elem =
      if List.exists (List.exists is_empty_line) groups then fun lines ->
        LiP (trans_from_lines ~unsafe lines)
      else fun lines -> Li (trans_spans ~unsafe lines)
    in
    List.map (fun group -> List.map remove_indent group |> trans_elem) groups
  in
  let is_xl_indent line =
    is_indent_start line
    || String.is_prefix line ~prefix:" "
    || is_empty_line line
  in
  fun ~unsafe lines ->
    match lines with
    | line :: lines' when is_indent_start line -> (
        match
          List.split_by_first lines' ~f:(fun line -> not (is_xl_indent line))
        with
        | None -> Some (constructor (trans_xl_elems ~unsafe lines), [])
        | Some (xl, cont_line, cont_lines) ->
            Some
              ( constructor (trans_xl_elems ~unsafe (line :: xl)),
                cont_line :: cont_lines ) )
    | _ -> None

and try_ul ~unsafe lines =
  gen_try_xl (fun x -> Ul x) is_ul_indent_start remove_ul_indent ~unsafe lines

and try_ol ~unsafe lines =
  gen_try_xl (fun x -> Ol x) is_ol_indent_start remove_ol_indent ~unsafe lines

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
  fun ~unsafe lines ->
    match lines with
    | line :: lines' when is_quote_indent line -> (
        match List.split_by_first lines' ~f:is_empty_line with
        | None ->
            Some (Quote (remove_indent lines |> trans_from_lines ~unsafe), [])
        | Some (quote, _, lines) ->
            Some
              ( Quote (remove_indent (line :: quote) |> trans_from_lines ~unsafe),
                lines ) )
    | _ -> None

and trans_from_lines ~unsafe lines =
  let rec trans lines rev =
    match List.remove_head lines ~f:is_empty_line with
    | [] -> List.rev rev
    | line :: _ as lines ->
        let ( >>= ) = gen_bind lines in
        None
        >>= ( if 0 < String.length line then
              match line.[0] with
              | '!' -> Unsafe.(try_block ~unsafe img)
              | '`' ->
                  fun _lines ->
                    None
                    >>= Unsafe.(try_block ~unsafe code_block)
                    >>= try_code_block_by_bound
              | '~' -> try_code_block_by_bound
              | ' ' -> try_code_block_by_indent
              | '<' ->
                  fun _lines ->
                    None
                    >>= Unsafe.(try_block ~unsafe div)
                    >>= Unsafe.(try_block ~unsafe script)
              | '*' -> fun _lines -> None >>= try_hr >>= try_ul ~unsafe
              | '#' -> try_header_by_sharp ~unsafe
              | '>' -> try_quote ~unsafe
              | c when Char.is_num c -> try_ol ~unsafe
              | _ -> fun _lines -> None
            else fun _lines -> None )
        >>= try_header_by_dash ~unsafe >>= try_p ~unsafe |> Option.value_exn
        |> fun (r, lines) -> (trans [@tailcall]) lines (r :: rev)
  in
  trans lines []

let trans ?(unsafe = false) s =
  String.split_to_lines s |> trans_from_lines ~unsafe

let trans_to_string ?unsafe s = trans ?unsafe s |> F.asprintf "%a" pp
