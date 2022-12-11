let split_to_lines =
  let re = Str.regexp "\r?\n" in
  fun s -> Str.split_delim re s

let trans ?(unsafe = false) s = split_to_lines s |> Trans.run ~unsafe

let get_input_from_channel ch =
  let buf = Buffer.create 256 in
  (try
     while true do
       Buffer.add_char buf (input_char ch)
     done
   with End_of_file -> ());
  Buffer.contents buf

let trans_from_file ?unsafe file =
  let ch = open_in file in
  let input = get_input_from_channel ch in
  close_in_noerr ch;
  trans ?unsafe input

let trans_from_stdin ?unsafe () = trans ?unsafe (get_input_from_channel stdin)

let trans_to_string ?unsafe ?rss s =
  trans ?unsafe s |> Format.asprintf "%a" (Typ.pp ?rss)
