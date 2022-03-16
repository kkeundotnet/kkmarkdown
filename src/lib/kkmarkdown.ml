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
  | StrikeOpen
  | StrikeClose
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

let pp_list ?(pp_sep = fun _f -> ()) pp f l =
  let rec aux = function
    | [] -> ()
    | [ x ] -> pp f x
    | hd :: tl ->
        pp f hd;
        pp_sep f;
        aux tl
  in
  aux l

let pp_char f = function
  | '&' -> F.pp_print_string f "&amp;"
  | '<' -> F.pp_print_string f "&lt;"
  | '>' -> F.pp_print_string f "&gt;"
  | '"' -> F.pp_print_string f "&quot;"
  | '\'' -> F.pp_print_string f "&apos;"
  | c -> F.pp_print_char f c

let pp_chars f s = String.iter (pp_char f) s

let pp_classes f classes =
  pp_list ~pp_sep:(fun f -> F.pp_print_char f ' ') F.pp_print_string f classes

let pp_open ?classes f tag =
  match classes with
  | None -> F.fprintf f {|<%s>|} tag
  | Some classes -> F.fprintf f {|<%s class="%a">|} tag pp_classes classes

let pp_close f tag = F.fprintf f "</%s>" tag

let pp_wrap tag ?classes pp f x =
  (match classes with
  | None -> pp_open f tag
  | Some classes -> pp_open ~classes f tag);
  pp f x;
  pp_close f tag

let pp_list_with_line f = pp_list ~pp_sep:(fun f -> F.pp_print_char f '\n') f

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
  | StrikeOpen -> pp_open f "s"
  | StrikeClose -> pp_close f "s"
  | CodeSpan code -> pp_wrap "code" (pp_list_with_line pp_chars) f code
  | A s when String.starts_with s ~prefix:"https://" ->
      F.fprintf f {|<a href="%s">%a</a>|} s pp_chars (Str.string_after s 8)
  | A s -> F.fprintf f {|<a href="%s">%a</a>|} s pp_chars s
  | UnsafeA { spans; link } ->
      F.fprintf f {|<a href="%s">%a</a>|} link pp_span_list spans

and pp_span_list f = pp_list pp_span f

let rec pp_block ~rss f = function
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
  | Quote quote -> pp_wrap "blockquote" (pp ~rss) f quote
  | Ol lis -> pp_wrap "ol" (pp_list_with_line (pp_li ~rss)) f lis
  | Ul lis -> pp_wrap "ul" (pp_list_with_line (pp_li ~rss)) f lis
  | UnsafeCodeBlock { cb; classes } ->
      let pp_wrap_code pp f x =
        if rss then pp_wrap "code" pp f x else pp_wrap "code" ~classes pp f x
      in
      pp_wrap "pre" (pp_wrap_code (pp_list_with_line pp_chars)) f cb
  | UnsafeInlineHtml lines ->
      if not rss then
        pp_list
          ~pp_sep:(fun f -> F.pp_print_char f '\n')
          F.pp_print_string f lines
  | UnsafeImg { alt; link; classes } ->
      let pp_class_prop f classes =
        if not rss then F.fprintf f {| class="%a"|} pp_classes classes
      in
      let pp_img f () =
        F.fprintf f {|<img alt="%s" src="%s"%a>|} alt link pp_class_prop classes
      in
      pp_wrap "p" pp_img f ()

and pp_li ~rss f = function
  | Li sps -> pp_wrap "li" pp_span_list f sps
  | LiP blocks -> pp_wrap "li" (pp ~rss) f blocks

and pp ?(rss = false) f = pp_list_with_line (pp_block ~rss) f

(* >=> *)

let ( >=> ) f g x = match f x with Some _ as r -> r | None -> g x

(* Continuation status *)

type status = InEm | InStrong | InEmStrong | InStrike

let status_equal x y = x = y

type spans_cont = { cur : int; status : status list; lines : string list }

(* Local utils *)

let get_input_from_channel ch =
  let buf = Buffer.create 256 in
  (try
     while true do
       Buffer.add_char buf (input_char ch)
     done
   with End_of_file -> ());
  Buffer.contents buf

let is_empty_line =
  let re = Str.regexp "[ \t]*$" in
  fun line -> Str.string_match re line 0

let split_by_first x ~f =
  let rec aux x ~f rev_l =
    match x with
    | [] -> None
    | hd :: tl ->
        if f hd then Some (List.rev rev_l, hd, tl) else aux tl ~f (hd :: rev_l)
  in
  aux x ~f []

let split_by_first_char c lines =
  let rec aux lines rev_l =
    match lines with
    | [] -> None
    | line :: lines -> (
        match String.index_opt line c with
        | None -> aux lines (line :: rev_l)
        | Some cur ->
            Some (List.rev (String.sub line 0 cur :: rev_l), cur, line :: lines)
        )
  in
  aux lines []

let trim x =
  let rec aux rev = function
    | [] -> rev
    | hd :: tl ->
        if is_empty_line hd then aux rev tl else List.rev_append tl (hd :: rev)
  in
  aux [] x |> aux []

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

  let read_classes =
    let re_space = Str.regexp "[ \t]+" in
    let re_class = Str.regexp "\\.\\(.+\\)" in
    fun s ->
      String.trim s |> Str.split re_space
      |> List.filter_map (fun class_ ->
             if Str.string_match re_class class_ 0 then
               Some (Str.matched_group 1 class_)
             else None)

  (* ![alt](link) or ![alt](link) {.class1 .class2} *)
  let img =
    let common = "!\\[\\(.*\\)\\](\\(.*\\))[ \t]*" in
    let re_without_class = Str.regexp (common ^ "$") in
    let re_with_class = Str.regexp (common ^ "{\\(.*\\)}[ \t]*$") in
    let get_alt_link matched_line =
      ( Str.matched_group 1 matched_line |> String.trim,
        Str.matched_group 2 matched_line |> String.trim )
    in
    function
    | line :: lines when Str.string_match re_without_class line 0 ->
        let alt, link = get_alt_link line in
        Some (UnsafeImg { alt; link; classes = [] }, lines)
    | line :: lines when Str.string_match re_with_class line 0 ->
        let alt, link = get_alt_link line in
        let classes = Str.matched_group 3 line |> read_classes in
        Some (UnsafeImg { alt; link; classes }, lines)
    | _ -> None

  (* ``` {.class1 .class2} or ~~~ {.class1 .class2} *)
  let code_block =
    let re1 = Str.regexp "\\(```+\\)[ \t]*{\\(.*\\)}[ \t]*$" in
    let re2 = Str.regexp "\\(~~~+\\)[ \t]*{\\(.*\\)}[ \t]*$" in
    function
    | line :: lines
      when Str.string_match re1 line 0 || Str.string_match re2 line 0 ->
        let code_block_bound = Str.matched_group 1 line in
        let classes = Str.matched_group 2 line |> read_classes in
        let cb, lines =
          match
            split_by_first lines ~f:(fun s ->
                String.equal (String.trim s) code_block_bound)
          with
          | None -> (lines, [])
          | Some (cb, _, lines) -> (cb, lines)
        in
        Some (UnsafeCodeBlock { cb; classes }, lines)
    | _ -> None

  let gen_inline_html ~tag =
    let re_start = Str.regexp ("<[ \t]*" ^ tag ^ "[ >]") in
    let re_end = Str.regexp ("</[ \t]*" ^ tag ^ "[ \t]*>[ \t]*$") in
    function
    | line :: lines as all when Str.string_match re_start line 0 ->
        let inline_html, lines =
          match
            split_by_first lines ~f:(fun line -> Str.string_match re_end line 0)
          with
          | None -> (all, [])
          | Some (div_body, div_close, lines) ->
              (line :: (div_body @ [ div_close ]), lines)
        in
        Some (UnsafeInlineHtml inline_html, lines)
    | _ -> None

  (* <div> *)
  let div = gen_inline_html ~tag:"div"

  (* <script> *)
  let script = gen_inline_html ~tag:"script"

  (* [text](link) *)
  let a =
    let re = Str.regexp "\\[\\([^]]*\\)\\](\\([^)]*\\))" in
    fun ~trans_spans_of_line { cur; status; lines } ->
      match lines with
      | line :: _ when Str.string_match re line cur ->
          let text = Str.matched_group 1 line in
          let link = Str.matched_group 2 line |> String.trim in
          Some
            ( UnsafeA { spans = trans_spans_of_line text; link },
              { cur = Str.match_end (); status; lines } )
      | _ -> None
end

(* Parse spans *)

let try_escape_char =
  let re = Str.regexp "\\\\\\([]\\\\`\\*_{}[()#\\+\\.!-]\\)" in
  fun ({ cur; lines; _ } as cont) ->
    match lines with
    | line :: _ when Str.string_match re line cur ->
        let c = Str.matched_group 1 line in
        Some (CharSpan c.[0], { cont with cur = Str.match_end () })
    | _ -> None

let try_unicode =
  (* &#xhhhh; or &#xhhhhh; *)
  let re_hex =
    Str.regexp "&#x[0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F]?;"
  in
  (* &#nnnn; or &#nnnnn; or &#nnnnnn; *)
  let re_dec = Str.regexp "&#[0-9][0-9][0-9][0-9][0-9]?[0-9]?;" in
  let get_unicode cur s =
    if Str.string_match re_hex s cur || Str.string_match re_dec s cur then
      Some (Str.matched_string s, Str.match_end ())
    else None
  in
  fun ({ cur; lines; _ } as cont) ->
    match lines with
    | line :: _ -> (
        match get_unicode cur line with
        | None -> None
        | Some (unicode, cur) -> Some (UnicodeSpan unicode, { cont with cur }))
    | [] -> None

let try_paren re in_paren ~open_ ~close { cur; status; lines } =
  match lines with
  | line :: _ when Str.string_match re line cur -> (
      let cur = Str.match_end () in
      match status with
      | hd :: tl when status_equal hd in_paren ->
          Some (close, { cur; status = tl; lines })
      | _ -> Some (open_, { cur; status = in_paren :: status; lines }))
  | _ -> None

let try_em =
  let re = Str.regexp "\\*" in
  try_paren re InEm ~open_:EmOpen ~close:EmClose

let try_strong =
  let re = Str.regexp "\\*\\*" in
  try_paren re InStrong ~open_:StrongOpen ~close:StrongClose

let try_em_strong =
  let re = Str.regexp "\\*\\*\\*" in
  try_paren re InEmStrong ~open_:EmStrongOpen ~close:EmStrongClose

let try_strike =
  let re = Str.regexp "~~" in
  try_paren re InStrike ~open_:StrikeOpen ~close:StrikeClose

let try_code ({ cur; lines; _ } as cont) =
  match lines with
  | line :: lines when cur < String.length line && Char.equal line.[cur] '`'
    -> (
      let lines = Str.string_after line (cur + 1) :: lines in
      match split_by_first_char '`' lines with
      | None -> Some (CodeSpan lines, { cont with cur = 0; lines = [] })
      | Some (code, cur, lines) ->
          Some (CodeSpan code, { cont with cur = cur + 1; lines }))
  | _ -> None

let try_br =
  let re = Str.regexp "  +$" in
  fun ({ cur; lines; _ } as cont) ->
    match lines with
    | line :: _ when Str.string_match re line cur ->
        Some (Br, { cont with cur = Str.match_end () })
    | _ -> None

let try_char_span ({ cur; lines; _ } as cont) =
  match lines with
  | line :: lines' ->
      if cur < String.length line then
        let len = String.length line in
        let rec read_chars n =
          if n < len then
            match line.[n] with
            | '[' | '\\' | '&' | '*' | '~' | '`' | ' ' | '<' -> n
            | _ -> read_chars (n + 1)
          else n
        in
        let cur' = read_chars (cur + 1) in

        (CharsSpan (String.sub line cur (cur' - cur)), { cont with cur = cur' })
      else
        let r = match lines' with [] -> NoneSpan | _ :: _ -> CharSpan '\n' in
        (r, { cont with cur = 0; lines = lines' })
  | [] -> (NoneSpan, cont)

let try_a =
  let re = Str.regexp "<\\(https?://[^\"'>]*\\)>" in
  fun ({ cur; lines; _ } as cont) ->
    match lines with
    | line :: _ when Str.string_match re line cur ->
        let link = Str.matched_group 1 line in
        Some (A link, { cont with cur = Str.match_end () })
    | _ -> None

let rec trans_spans ~unsafe =
  let rec close_status rev = function
    | [] -> rev
    | InEm :: status -> close_status (EmClose :: rev) status
    | InStrong :: status -> close_status (StrongClose :: rev) status
    | InEmStrong :: status -> close_status (EmStrongClose :: rev) status
    | InStrike :: status -> close_status (StrikeClose :: rev) status
  in
  let rec trans ({ status; lines; cur } as cont) rev =
    match lines with
    | [] -> close_status rev status |> List.rev
    | line :: _ ->
        let res =
          if cur < String.length line then
            match line.[cur] with
            | '[' ->
                Unsafe.(
                  try_span ~unsafe
                    (a ~trans_spans_of_line:(trans_spans_of_line ~unsafe)))
                  cont
            | '\\' -> try_escape_char cont
            | '&' -> try_unicode cont
            | '*' -> (try_em_strong >=> try_strong >=> try_em) cont
            | '~' -> try_strike cont
            | '`' -> try_code cont
            | ' ' -> try_br cont
            | '<' -> try_a cont
            | _ -> None
          else None
        in
        let span, cont =
          match res with Some r -> r | None -> try_char_span cont
        in
        (trans [@tailcall]) cont (span :: rev)
  in
  fun lines -> trans { cur = 0; status = []; lines } []

and trans_spans_of_line ~unsafe line = trans_spans ~unsafe [ line ]

(* Parse blocks *)

let try_hr =
  let re = Str.regexp "\\*\\*\\*+[ \t]*$" in
  function
  | line :: lines when Str.string_match re line 0 -> Some (Hr, lines)
  | _ -> None

let try_header_by_sharp =
  let re = Str.regexp "\\(#+\\) " in
  fun ~unsafe -> function
    | line :: lines when Str.string_match re line 0 -> (
        let title () =
          Str.string_after line (Str.match_end ())
          |> trans_spans_of_line ~unsafe
        in
        match Str.matched_group 1 line |> String.length with
        | 1 -> Some (H1 (title ()), lines)
        | 2 -> Some (H2 (title ()), lines)
        | 3 -> Some (H3 (title ()), lines)
        | 4 -> Some (H4 (title ()), lines)
        | 5 -> Some (H5 (title ()), lines)
        | 6 -> Some (H6 (title ()), lines)
        | _ -> None)
    | _ -> None

let try_header_by_dash =
  let re1 = Str.regexp "===+[ \t]*$" in
  let re2 = Str.regexp "---+[ \t]*$" in
  fun ~unsafe -> function
    | line1 :: line2 :: lines when Str.string_match re1 line2 0 ->
        Some (H1 (trans_spans_of_line ~unsafe line1), lines)
    | line1 :: line2 :: lines when Str.string_match re2 line2 0 ->
        Some (H2 (trans_spans_of_line ~unsafe line1), lines)
    | _ -> None

let try_code_block_by_bound =
  let re1 = Str.regexp "```+[ \t]*$" in
  let re2 = Str.regexp "~~~+[ \t]*$" in
  function
  | line :: lines
    when Str.string_match re1 line 0 || Str.string_match re2 line 0 -> (
      let bound = Str.matched_string line |> String.trim in
      match
        split_by_first lines ~f:(fun s -> String.equal (String.trim s) bound)
      with
      | None -> Some (CodeBlock lines, [])
      | Some (cb, _, lines) -> Some (CodeBlock cb, lines))
  | _ -> None

let try_code_block_by_indent =
  let is_code_block_indent line = String.starts_with line ~prefix:"    " in
  let remove_indent lines = List.map (fun s -> Str.string_after s 4) lines in
  function
  | line :: lines as x when is_code_block_indent line -> (
      match
        split_by_first lines ~f:(fun line -> not (is_code_block_indent line))
      with
      | None -> Some (CodeBlock (remove_indent x), [])
      | Some (cb, cont_line, cont_lines) ->
          Some (CodeBlock (remove_indent (line :: cb)), cont_line :: cont_lines)
      )
  | _ -> None

let try_p ~unsafe lines =
  let p, lines =
    match split_by_first lines ~f:is_empty_line with
    | None -> (lines, [])
    | Some (p, _, lines) -> (p, lines)
  in
  (P (trans_spans ~unsafe p), lines)

let block_max_depth = 4
let re_ul = Str.regexp "* \\(  \\| \\|\\)"
let re_ol = Str.regexp "[0-9]\\(.  ?\\|[0-9]. \\)"
let is_xl_item_start re_xl line = Str.string_match re_xl line 0
let is_ul_item_start = is_xl_item_start re_ul
let is_ol_item_start = is_xl_item_start re_ol

let remove_xl_indent =
  let re_spaces_upto_4 = Str.regexp "    \\|   \\|  \\| " in
  fun re_xl line ->
    if Str.string_match re_xl line 0 || Str.string_match re_spaces_upto_4 line 0
    then Str.string_after line (Str.match_end ())
    else line

let remove_ul_indent = remove_xl_indent re_ul
let remove_ol_indent = remove_xl_indent re_ol

let rec gen_try_xl constructor is_item_start remove_indent =
  let divide_groups =
    let rec aux l rev_g rev_gs =
      match (l, rev_g) with
      | [], None -> List.rev rev_gs
      | [], Some rev_g -> List.rev (List.rev rev_g :: rev_gs)
      | hd :: tl, None -> aux tl (Some [ hd ]) rev_gs
      | hd :: tl, Some rev_g ->
          let rev_g, rev_gs =
            if is_item_start hd then ([ hd ], List.rev rev_g :: rev_gs)
            else (hd :: rev_g, rev_gs)
          in
          aux tl (Some rev_g) rev_gs
    in
    fun x -> aux x None []
  in
  let trans_xl_elems ~unsafe ~depth lines =
    let groups = trim lines |> divide_groups in
    let trans_elem =
      if List.exists (List.exists is_empty_line) groups then fun lines ->
        LiP (trans_from_lines ~unsafe ~depth lines)
      else fun lines -> Li (trans_spans ~unsafe lines)
    in
    List.map (fun group -> List.map remove_indent group |> trans_elem) groups
  in
  let is_xl_line line =
    is_item_start line
    || String.starts_with line ~prefix:" "
    || is_empty_line line
  in
  fun ~unsafe ~depth lines ->
    if depth > block_max_depth then None
    else
      let depth = depth + 1 in
      match lines with
      | line :: lines' when is_item_start line ->
          let xl_lines, cont_lines =
            match
              split_by_first lines' ~f:(fun line -> not (is_xl_line line))
            with
            | None -> (lines, [])
            | Some (xl, cont_line, cont_lines) ->
                (line :: xl, cont_line :: cont_lines)
          in
          Some (constructor (trans_xl_elems ~unsafe ~depth xl_lines), cont_lines)
      | _ -> None

and try_ul ~unsafe ~depth lines =
  gen_try_xl
    (fun x -> Ul x)
    is_ul_item_start remove_ul_indent ~unsafe ~depth lines

and try_ol ~unsafe ~depth lines =
  gen_try_xl
    (fun x -> Ol x)
    is_ol_item_start remove_ol_indent ~unsafe ~depth lines

and try_quote =
  let remove_indent =
    let re = Str.regexp "> ?\\|  ?" in
    fun lines ->
      let remove_indent line =
        if Str.string_match re line 0 then
          Str.string_after line (Str.match_end ())
        else line
      in
      List.map remove_indent lines
  in
  fun ~unsafe ~depth lines ->
    if depth > block_max_depth then None
    else
      let depth = depth + 1 in
      match lines with
      | line :: lines' when String.starts_with line ~prefix:"> " ->
          let quote_lines, cont_lines =
            match split_by_first lines' ~f:is_empty_line with
            | None -> (lines, [])
            | Some (quote, _, lines) -> (line :: quote, lines)
          in
          Some
            ( Quote
                (remove_indent quote_lines |> trans_from_lines ~unsafe ~depth),
              cont_lines )
      | _ -> None

and trans_from_lines ~unsafe ~depth lines =
  let rec trans lines rev =
    match trim lines with
    | [] -> List.rev rev
    | line :: _ as lines ->
        let res =
          (if 0 < String.length line then
           match line.[0] with
           | '!' -> Unsafe.(try_block ~unsafe img) lines
           | '`' | '~' ->
               (Unsafe.(try_block ~unsafe code_block)
               >=> try_code_block_by_bound)
                 lines
           | ' ' -> try_code_block_by_indent lines
           | '<' ->
               (Unsafe.(try_block ~unsafe div)
               >=> Unsafe.(try_block ~unsafe script))
                 lines
           | '*' -> (try_hr >=> try_ul ~unsafe ~depth) lines
           | '#' -> try_header_by_sharp ~unsafe lines
           | '>' -> try_quote ~unsafe ~depth lines
           | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ->
               try_ol ~unsafe ~depth lines
           | _ -> None
          else None)
          |> function
          | Some _ as r -> r
          | None -> try_header_by_dash ~unsafe lines
        in
        let r, lines =
          match res with Some res -> res | None -> try_p ~unsafe lines
        in
        (trans [@tailcall]) lines (r :: rev)
  in
  trans lines []

let split_to_lines =
  let re = Str.regexp "\r?\n" in
  fun s -> Str.split_delim re s

let trans ?(unsafe = false) s =
  split_to_lines s |> trans_from_lines ~unsafe ~depth:0

let trans_from_file ?unsafe file =
  let ch = open_in file in
  let input = get_input_from_channel ch in
  close_in_noerr ch;
  trans ?unsafe input

let trans_from_stdin ?unsafe () = trans ?unsafe (get_input_from_channel stdin)

let trans_to_string ?unsafe ?rss s =
  trans ?unsafe s |> F.asprintf "%a" (pp ?rss)
