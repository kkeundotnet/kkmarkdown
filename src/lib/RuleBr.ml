let re = Str.regexp ".*  +$"

let remove_trailing_empty_spaces line =
  let len = String.length line in
  let exception Done of string in
  try
    for i = len - 1 downto 0 do
      if line.[i] <> ' ' then raise (Done (String.sub line 0 (i + 1)))
    done;
    ""
  with Done s -> s

(** Separate lines by {!re} *)
let separate lines =
  List.fold_left
    (fun acc line ->
      match acc with
      | [] -> assert false
      | hd :: tl ->
          if Str.string_match re line 0 then
            let line = remove_trailing_empty_spaces line in
            [] :: (line :: hd) :: tl
          else (line :: hd) :: tl)
    [ [] ] lines

(** Merge lines with space as a separator *)
let merge_lines lines =
  let rec length lines acc =
    match lines with
    | [] -> 0
    | [ last ] -> String.length last
    | hd :: tl -> length tl (String.length hd + 1)
  in
  let buffer = Buffer.create (length lines 0) in
  let rec write_buffer lines =
    match lines with
    | [] -> ()
    | [ last ] -> Buffer.add_string buffer last
    | hd :: tl ->
        Buffer.add_string buffer hd;
        Buffer.add_char buffer ' ';
        write_buffer tl
  in
  write_buffer lines;
  Buffer.contents buffer

(** Merge spans with {!Typ.Br} as a separator *)
let merge_spans spans_list =
  let res = Queue.create () in
  let push_spans spans = List.iter (fun span -> Queue.push span res) spans in
  let rec push spans_list =
    match spans_list with
    | [] -> ()
    | [ last ] -> push_spans last
    | hd :: tl ->
        push_spans hd;
        Queue.push Typ.Br res;
        push tl
  in
  push spans_list;
  List.of_seq (Queue.to_seq res)

let construct trans_spans lines =
  separate lines
  |> List.rev_map (fun rev_lines ->
         let lines = List.rev rev_lines in
         trans_spans (merge_lines lines))
  |> merge_spans
