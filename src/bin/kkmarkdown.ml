module F = Format
module Kkmarkdown = Kkmarkdown_lib.Kkmarkdown

let get_input_from_channel ch =
  let buf = Buffer.create 256 in
  ( try
      while true do
        Buffer.add_char buf (input_char ch)
      done
    with End_of_file -> () );
  Buffer.contents buf

let get_input_from_stdin () = get_input_from_channel stdin

let get_input_from_file file =
  let ch = open_in file in
  let input = get_input_from_channel ch in
  close_in_noerr ch;
  input

let main () =
  let input =
    if Array.length Sys.argv >= 2 then get_input_from_file Sys.argv.(1)
    else get_input_from_stdin ()
  in
  Kkmarkdown.trans input |> Kkmarkdown.pp F.std_formatter;
  F.print_flush ()

let () = main ()
