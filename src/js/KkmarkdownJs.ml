open Js_of_ocaml
module Kkmarkdown = Kkmarkdown_lib.Kkmarkdown

let () =
  Js.export "kkmarkdown"
    (object%js
       val trans =
         fun s -> Js.string (Kkmarkdown.trans_to_string (Js.to_string s))

       (* Note: Do not use underscore in the method name.
          https://ocsigen.org/js_of_ocaml/2.4.1/manual/library.html *)
       val unsafe =
         fun s ->
           Js.string (Kkmarkdown.trans_to_string ~unsafe:true (Js.to_string s))
    end)
