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
