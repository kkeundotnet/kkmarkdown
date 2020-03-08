module F = Format
module S = Sdown_lib.Sdown

let trans input = F.asprintf "%a" S.pp (S.trans input)

let check msg expecting input =
  Alcotest.(check string) msg expecting (trans input)

let test_empty () =
  check "empty" "" "" ; check "empty" "" "\n" ; check "empty" "" "\n\n"

let test_escape () =
  check "escape" "<p>&amp;&lt;&gt;&quot;&apos;</p>" {|&<>"'|}

let test_hr () = check "hr" "<hr>" "***" ; check "hr" "<hr>" "******"

let tests =
  [ ("empty", `Quick, test_empty)
  ; ("escape", `Quick, test_escape)
  ; ("hr", `Quick, test_hr) ]
