module F = Format
module Kkmarkdown = Kkmarkdown_lib.Kkmarkdown

let get_input_from_channel ch =
  let rev_lines = ref [] in
  ( try
      while true do
        rev_lines := input_line ch :: !rev_lines
      done
    with End_of_file -> () );
  (* NOTE: It concatenates "lines" to a string, then split them again
     in [Kkmarkdown.trans].  This inefficient way is to address the
     cariage return. *)
  List.rev !rev_lines |> String.concat "\n"

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
