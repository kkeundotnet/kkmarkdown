include Stdlib.String

let for_all s ~f =
  let exception Failed in
  try
    iter (fun c -> if f c then () else raise Failed) s ;
    true
  with Failed -> false
