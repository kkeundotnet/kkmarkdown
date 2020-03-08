(* TODO rename *)
module F = Format

(* TODO
block elements
- paragraph
- headers
- block quote
- lists
- code blocks
- horizontal rules

span elements
- line breaks
- emphasis
- code

later
- restricted links
 *)

type span = V of string | Br | Em of span | Strong of span | CodeSpan

type block =
  | P of span list
  | H1
  | H2
  | H3
  | H4
  | H5
  | H6
  | Quote
  | Lists
  | CodeBlock
  | Hr

type t = block list

module EscapeMap = Map.Make (Char)

let escape_map =
  EscapeMap.of_list
    [ ('&', "&amp;")
    ; ('<', "&lt;")
    ; ('>', "&gt;")
    ; ('"', "&quot;")
    ; ('\'', "&apos;") ]

let pp_escape_char f c =
  match EscapeMap.find_opt c escape_map with
  | Some s ->
      F.pp_print_string f s
  | None ->
      F.pp_print_char f c

let pp_unicode_format f s n =
  if
    (* &#xhhhh; *)
    n + 7 < String.length s
    && s.[n] = '&'
    && s.[n + 1] = '#'
    && s.[n + 2] = 'x'
    && Char.is_hexa s.[n + 3]
    && Char.is_hexa s.[n + 4]
    && Char.is_hexa s.[n + 5]
    && Char.is_hexa s.[n + 6]
    && s.[n + 7] = ';'
  then (
    let sub = String.sub s n 8 in
    F.pp_print_string f sub ; Some 8 )
  else if
    (* &#nnnn; *)
    n + 6 < String.length s
    && s.[n] = '&'
    && s.[n + 1] = '#'
    && Char.is_num s.[n + 2]
    && Char.is_num s.[n + 3]
    && Char.is_num s.[n + 4]
    && Char.is_num s.[n + 5]
    && s.[n + 6] = ';'
  then (
    let sub = String.sub s n 7 in
    F.pp_print_string f sub ; Some 7 )
  else None

let pp_escape f s =
  let rec pp_escape f s n =
    if n < String.length s then
      match pp_unicode_format f s n with
      | None ->
          pp_escape_char f s.[n] ;
          pp_escape f s (n + 1)
      | Some m ->
          pp_escape f s (n + m)
  in
  pp_escape f s 0

let pp_wrap f tag pp x = F.fprintf f "<%s>%a</%s>" tag pp x tag

let pp_list pp f l = List.iter (pp f) l

let rec pp_span f = function
  | V s ->
      pp_escape f s
  | Br ->
      F.pp_print_string f "<br>"
  | Em sp ->
      pp_wrap f "em" pp_span sp
  | Strong sp ->
      pp_wrap f "strong" pp_span sp
  | CodeSpan ->
      assert false

let pp_span_list = pp_list pp_span

let pp_block f = function
  | P sps ->
      pp_wrap f "p" pp_span_list sps
  | Hr ->
      F.pp_print_string f "<hr>"
  | _ ->
      assert false

let pp = pp_list pp_block

let is_hr line =
  String.length line >= 3 && String.for_all line ~f:(fun c -> c = '*')

let trans_lines lines =
  let rec trans lines rev =
    match lines with
    | [] ->
        List.rev rev
    | line :: lines when is_hr line ->
        trans lines (Hr :: rev)
    | line :: lines ->
        trans lines (P [V line] :: rev)
  in
  trans lines []

let trans s = trans_lines (String.split_on_char '\n' s)
