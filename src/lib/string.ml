include Stdlib.String

let foralli s ~f =
  let exception Failed in
  try
    iteri (fun i c -> if f i c then () else raise Failed) s ;
    true
  with Failed -> false

let forall s ~f = foralli s ~f:(fun _i c -> f c)

let is_prefix line ~prefix =
  if length line < length prefix then false
  else foralli prefix ~f:(fun i c -> get line i = c)

let sub_from s n = sub s n (length s - n)
