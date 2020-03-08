(*
   Run all the OCaml test suites defined in the project.
*)

let test_suites : unit Alcotest.test list =
  [("Sdown_test.A", Sdown_test.A.tests)]

let () = Alcotest.run "Sdown" test_suites
