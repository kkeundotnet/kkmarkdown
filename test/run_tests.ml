(*
   Run all the OCaml test suites defined in the project.
*)

let test_suites : unit Alcotest.test list =
  [ ("Kkmarkdown_test.A", Kkmarkdown_test.A.tests) ]

let () = Alcotest.run "Kkmarkdown" test_suites
