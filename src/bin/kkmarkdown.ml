open Kkmarkdown_lib
module F = Format

type env = { rss : bool; unsafe : bool; input_files : string list }

let parse_arg () =
  let rss = ref false in
  let unsafe = ref false in
  let input_files_rev = ref [] in
  let speclist =
    [
      ("--rss", Arg.Set rss, "Enable rss mode");
      ("--unsafe", Arg.Set unsafe, "Enable unsafe mode");
    ]
  in
  Arg.parse speclist
    (fun file -> input_files_rev := file :: !input_files_rev)
    {|Usage: kkmarkdown [OPTION]... [FILE]
If FILE is not given, it reads input from stdin.  Visit https://github.com/kkeundotnet/kkmarkdown
for more information.
|};
  { rss = !rss; unsafe = !unsafe; input_files = List.rev !input_files_rev }

let get_input_from_stdin () = Util.get_input_from_channel stdin

let main { rss; unsafe; input_files } =
  ( match input_files with
  | [] -> Kkmarkdown.trans ~unsafe (get_input_from_stdin ())
  | [ file ] -> Kkmarkdown.trans_from_file ~unsafe file
  | _ -> invalid_arg "Multiple files are given." )
  |> Kkmarkdown.pp ~rss F.std_formatter;
  F.print_flush ()

let () = parse_arg () |> main
