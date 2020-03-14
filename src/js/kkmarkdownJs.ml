open Js_of_ocaml
module Kkmarkdown = Kkmarkdown_lib.Kkmarkdown

let () =
  Js.export_all
    (object%js
       val trans = Kkmarkdown.trans
    end)
