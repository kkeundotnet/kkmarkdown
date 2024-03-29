module F = Format
module Kkmarkdown = Lib.Kkmarkdown
module Typ = Lib.Typ

type env = { rss : bool; unsafe : bool; input_files : string list }

let show_version () =
  let version =
    match Build_info.V1.version () with
    | None -> "n/a"
    | Some v -> Build_info.V1.Version.to_string v
  in
  print_endline version;
  exit 0

let parse_arg () =
  let rss = ref false in
  let unsafe = ref false in
  let input_files_rev = ref [] in
  let speclist =
    [
      ("--rss", Arg.Set rss, "Enable RSS mode");
      ("--unsafe", Arg.Set unsafe, "Enable unsafe mode");
      ("--version", Arg.Unit show_version, "Show version");
    ]
  in
  Arg.parse speclist
    (fun file -> input_files_rev := file :: !input_files_rev)
    {|Usage: kkmarkdown [OPTION]... [FILE]

If FILE is not given, it reads input from stdin.  Visit https://github.com/kkeundotnet/kkmarkdown
for more information.
|};
  { rss = !rss; unsafe = !unsafe; input_files = List.rev !input_files_rev }

let main { rss; unsafe; input_files } =
  (match input_files with
  | [] -> Kkmarkdown.trans_from_stdin ~unsafe ()
  | [ file ] -> Kkmarkdown.trans_from_file ~unsafe file
  | _ -> invalid_arg "Multiple files are given.")
  |> Typ.pp ~rss F.std_formatter;
  F.print_flush ()

let () = parse_arg () |> main
