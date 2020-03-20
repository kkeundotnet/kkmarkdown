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

let unsafe = ref false

let input_files_rev = ref []

let speclist = [ ("--unsafe", Arg.Set unsafe, "Enable unsafe mode") ]

let main () =
  let input_files = List.rev !input_files_rev in
  let input =
    match input_files with
    | [] -> get_input_from_stdin ()
    | [ file ] -> get_input_from_file file
    | _ -> invalid_arg "Multiple files are given."
  in
  Kkmarkdown.trans ~unsafe:!unsafe input |> Kkmarkdown.pp F.std_formatter;
  F.print_flush ()

let parse_arg () =
  Arg.parse speclist
    (fun file -> input_files_rev := file :: !input_files_rev)
    {|Usage: kkmarkdown [OPTION]... [FILE]
If FILE is not given, it reads input from stdin.  Visit https://github.com/kkeundotnet/kkmarkdown
for more information.
|}

let () =
  parse_arg ();
  main ()
