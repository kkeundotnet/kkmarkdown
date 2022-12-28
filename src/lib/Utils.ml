let is_empty_line =
  let re = Str.regexp "[ \t]*$" in
  fun line -> Str.string_match re line 0

let re_spaces_upto_four = Str.regexp "    \\|   \\|  \\| "

let read_classes =
  let re_space = Str.regexp "[ \t]+" in
  let re_class = Str.regexp "\\.\\(.+\\)" in
  fun s ->
    String.trim s |> Str.split re_space
    |> List.filter_map (fun class_ ->
           if Str.string_match re_class class_ 0 then
             Some (Str.matched_group 1 class_)
           else None)

let remove_trailing_chars cond s =
  let len = String.length s in
  let exception Done of string in
  try
    for i = len - 1 downto 0 do
      if not (cond s.[i]) then raise (Done (String.sub s 0 (i + 1)))
    done;
    ""
  with Done s -> s

let remove_trailing_spaces s =
  remove_trailing_chars (fun c -> c = ' ' || c = '\t') s

let remove_trailing_sharps s = remove_trailing_chars (fun c -> c = '#') s
