(* TODO rename *)
open Proj_sub2
module F = Format

let get_intput_from_stdin () =
  let rev = ref [] in
  ( try
      while true do
        rev := read_line () :: !rev
      done
    with End_of_file -> () ) ;
  List.rev !rev

let print_output_to_stdout bls =
  Sdown.pp F.std_formatter bls ;
  F.print_flush ()

let main () =
  let input = get_intput_from_stdin () in
  let output = Sdown.trans_lines input in
  print_output_to_stdout output

let () = main ()
