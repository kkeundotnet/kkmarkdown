module F = Format
module Kkmarkdown = Kkmarkdown_lib.Kkmarkdown

let get_intput_from_stdin () =
  let rev = ref [] in
  ( try
      while true do
        rev := read_line () :: !rev
      done
    with End_of_file -> () ) ;
  List.rev !rev

let print_output_to_stdout bls =
  Kkmarkdown.pp F.std_formatter bls ;
  F.print_flush ()

let main () =
  let input = get_intput_from_stdin () in
  let output = Kkmarkdown.trans_from_lines input in
  print_output_to_stdout output

let () = main ()
