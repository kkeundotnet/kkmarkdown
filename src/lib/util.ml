let get_input_from_channel ch =
  let buf = Buffer.create 256 in
  (try
     while true do
       Buffer.add_char buf (input_char ch)
     done
   with End_of_file -> ());
  Buffer.contents buf
