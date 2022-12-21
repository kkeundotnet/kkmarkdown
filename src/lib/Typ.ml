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
  | A s when String.starts_with s ~prefix:"https://" ->
      F.fprintf f {|<a href="%s">%a</a>|} s pp_chars (Str.string_after s 8)
  | A s -> F.fprintf f {|<a href="%s">%a</a>|} s pp_chars s
  | Br -> pp_open f "br"
  | CharSpan c -> pp_char f c
  | CharsSpan s -> pp_chars f s
  | StackOpen Em -> pp_open f "em"
  | StackClose Em -> pp_close f "em"
  | StackOpen Strong -> pp_open f "strong"
  | StackClose Strong -> pp_close f "strong"
  | StackOpen EmStrong ->
      pp_open f "em";
      pp_open f "strong"
  | StackClose EmStrong ->
      pp_close f "strong";
      pp_close f "em"
  | StackOpen Strike -> pp_open f "s"
  | StackClose Strike -> pp_close f "s"
  | StackOpen code -> pp_open f "code"
  | StackClose code -> pp_close f "code"
  | UnicodeSpan s -> F.pp_print_string f s
  | UnsafeA { spans; link } ->
      F.fprintf f {|<a href="%s">%a</a>|} link pp_span_list spans

and pp_span_list f = pp_list pp_span f

let rec pp_block ~rss f = function
  | CodeBlock code_block ->
      pp_wrap "pre" (pp_wrap "code" (pp_list_with_line pp_chars)) f code_block
  | H1 sps -> pp_wrap "h1" pp_span_list f sps
  | H2 sps -> pp_wrap "h2" pp_span_list f sps
  | H3 sps -> pp_wrap "h3" pp_span_list f sps
  | H4 sps -> pp_wrap "h4" pp_span_list f sps
  | H5 sps -> pp_wrap "h5" pp_span_list f sps
  | H6 sps -> pp_wrap "h6" pp_span_list f sps
  | Hr -> pp_open f "hr"
  | List_ (Ordered, lis) -> pp_wrap "ol" (pp_list_with_line (pp_li ~rss)) f lis
  | List_ (Unordered, lis) ->
      pp_wrap "ul" (pp_list_with_line (pp_li ~rss)) f lis
  | P sps -> pp_wrap "p" pp_span_list f sps
  | Quote quote -> pp_wrap "blockquote" (pp ~rss) f quote
  | UnsafeCodeBlock { cb; classes } ->
      let pp_wrap_code pp f x =
        if rss then pp_wrap "code" pp f x else pp_wrap "code" ~classes pp f x
      in
      pp_wrap "pre" (pp_wrap_code (pp_list_with_line pp_chars)) f cb
  | UnsafeImg { alt; link; classes } ->
      let pp_class_prop f classes =
        if not rss then F.fprintf f {| class="%a"|} pp_classes classes
      in
      let pp_img f () =
        F.fprintf f {|<img alt="%s" src="%s"%a>|} alt link pp_class_prop classes
      in
      pp_wrap "p" pp_img f ()
  | UnsafeInlineHtml lines ->
      if not rss then
        pp_list
          ~pp_sep:(fun f -> F.pp_print_char f '\n')
          F.pp_print_string f lines

and pp_li ~rss f = function
  | Li sps -> pp_wrap "li" pp_span_list f sps
  | LiP blocks -> pp_wrap "li" (pp ~rss) f blocks

and pp ?(rss = false) f = pp_list_with_line (pp_block ~rss) f
