module F = Format
module Kkmarkdown = Kkmarkdown_lib.Kkmarkdown

let get_intput_from_stdin () =
  let rev = ref [] in
  ( try
      while true do
        rev := read_line () :: !rev
      done
    with End_of_file -> () );
  List.rev !rev

let print_output_to_stdout bls =
  Kkmarkdown.pp F.std_formatter bls;
  F.print_flush ()

let main () =
  if Array.length Sys.argv >= 2 then (
    let rev_lines = ref [] in
    let ic = open_in Sys.argv.(1) in
    try
      while true do
        rev_lines := input_line ic :: !rev_lines
      done
    with End_of_file ->
      close_in_noerr ic;
      Kkmarkdown.trans_from_lines (List.rev !rev_lines)
      |> Kkmarkdown.pp F.std_formatter;
      F.print_flush () )
  else
    let input = get_intput_from_stdin () in
    let output = Kkmarkdown.trans_from_lines input in
    print_output_to_stdout output

let () = main ()
