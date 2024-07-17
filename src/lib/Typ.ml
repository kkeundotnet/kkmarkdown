module F = Format

type stack_elt = Em | Strong | EmStrong | Strike | Code

type span =
  | A of string
  | Br
  | CharSpan of char
  | CharsSpan of string
  | StackOpen of stack_elt
  | StackClose of stack_elt
  | UnicodeSpan of string
  | UnsafeA of { spans : span list; link : string }

type list_typ = Ordered | Unordered

type block =
  | CodeBlock of string list
  | Details of { title : span list; body : t }
  | H1 of span list
  | H2 of span list
  | H3 of span list
  | H4 of span list
  | H5 of span list
  | H6 of span list
  | Hr
  | List_ of list_typ * li list
  | P of span list
  | Quote of t
  | UnsafeCodeBlock of { cb : string list; classes : string list }
  | UnsafeImg of { alt : string; link : string; classes : string list }
  | UnsafeInlineHtml of string list

and li = Li of span list | LiP of t
and t = block list

let pp_nop _f = ()
let pp_print_char c f = F.pp_print_char f c
let pp_print_string s f = F.pp_print_string f s

let ( ++ ) pp1 pp2 f =
  pp1 f;
  pp2 f

let pp_list ?(pp_sep = pp_nop) pp l =
  let rec aux acc = function
    | [] -> acc
    | [ x ] -> acc ++ pp x
    | hd :: tl -> aux (acc ++ pp hd ++ pp_sep) tl
  in
  aux pp_nop l

let pp_char = function
  | '&' -> pp_print_string "&amp;"
  | '<' -> pp_print_string "&lt;"
  | '>' -> pp_print_string "&gt;"
  | '"' -> pp_print_string "&quot;"
  | '\'' -> pp_print_string "&apos;"
  | c -> pp_print_char c

let pp_chars s f = String.iter (fun c -> pp_char c f) s

let pp_classes classes =
  pp_list ~pp_sep:(pp_print_char ' ') pp_print_string classes

let pp_open ?classes tag =
  match classes with
  | None -> F.dprintf {|<%s>|} tag
  | Some classes -> F.dprintf {|<%s class="%t">|} tag (pp_classes classes)

let pp_close tag = F.dprintf "</%s>" tag
let pp_wrap tag ?classes pp = pp_open ?classes tag ++ pp ++ pp_close tag
let pp_list_with_line pp l = pp_list ~pp_sep:(pp_print_char '\n') pp l

let rec pp_span = function
  | A s ->
      let short_link =
        if String.starts_with s ~prefix:"https://" then Str.string_after s 8
        else s
      in
      F.dprintf {|<a href="%t">%t</a>|} (pp_chars s) (pp_chars short_link)
  | Br -> pp_open "br"
  | CharSpan c -> pp_char c
  | CharsSpan s -> pp_chars s
  | StackOpen Em -> pp_open "em"
  | StackClose Em -> pp_close "em"
  | StackOpen Strong -> pp_open "strong"
  | StackClose Strong -> pp_close "strong"
  | StackOpen EmStrong -> pp_open "em" ++ pp_open "strong"
  | StackClose EmStrong -> pp_close "strong" ++ pp_close "em"
  | StackOpen Strike -> pp_open "s"
  | StackClose Strike -> pp_close "s"
  | StackOpen code -> pp_open "code"
  | StackClose code -> pp_close "code"
  | UnicodeSpan s -> pp_print_string s
  | UnsafeA { spans; link } ->
      F.dprintf {|<a href="%t">%t</a>|} (pp_chars link) (pp_span_list spans)

and pp_span_list l = pp_list pp_span l

let rec pp_block ~rss = function
  | CodeBlock code_block ->
      pp_wrap "pre" (pp_wrap "code" (pp_list_with_line pp_chars code_block))
  | Details { title; body } ->
      pp_wrap "details" (pp_wrap "summary" (pp_span_list title) ++ pp ~rss body)
  | H1 sps -> pp_wrap "h1" (pp_span_list sps)
  | H2 sps -> pp_wrap "h2" (pp_span_list sps)
  | H3 sps -> pp_wrap "h3" (pp_span_list sps)
  | H4 sps -> pp_wrap "h4" (pp_span_list sps)
  | H5 sps -> pp_wrap "h5" (pp_span_list sps)
  | H6 sps -> pp_wrap "h6" (pp_span_list sps)
  | Hr -> pp_open "hr"
  | List_ (Ordered, lis) -> pp_wrap "ol" (pp_list_with_line (pp_li ~rss) lis)
  | List_ (Unordered, lis) -> pp_wrap "ul" (pp_list_with_line (pp_li ~rss) lis)
  | P sps -> pp_wrap "p" (pp_span_list sps)
  | Quote quote -> pp_wrap "blockquote" (pp ~rss quote)
  | UnsafeCodeBlock { cb; classes } ->
      let classes = if rss then None else Some classes in
      pp_wrap "pre" (pp_wrap "code" ?classes (pp_list_with_line pp_chars cb))
  | UnsafeImg { alt; link; classes } ->
      let pp_class_prop =
        if not rss then F.dprintf {| class="%t"|} (pp_classes classes)
        else pp_nop
      in
      let pp_img =
        F.dprintf {|<img alt="%t" src="%t"%t>|} (pp_chars alt) (pp_chars link)
          pp_class_prop
      in
      pp_wrap "p" pp_img
  | UnsafeInlineHtml lines ->
      if not rss then pp_list ~pp_sep:(pp_print_char '\n') pp_print_string lines
      else pp_nop

and pp_li ~rss = function
  | Li sps -> pp_wrap "li" (pp_span_list sps)
  | LiP blocks -> pp_wrap "li" (pp ~rss blocks)

and pp ~rss blocks = pp_list_with_line (pp_block ~rss) blocks

let pp ?(rss = false) f blocks = pp ~rss blocks f
