open Js_of_ocaml
module Kkmarkdown = Kkmarkdown_lib.Kkmarkdown

let () =
  Js.export "kkmarkdown"
    (object%js
       val trans =
         fun s -> Js.string (Kkmarkdown.trans_to_string (Js.to_string s))
    end)
