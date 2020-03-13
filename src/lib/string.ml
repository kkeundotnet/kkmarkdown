include Stdlib.String

let foralli s ~f =
  let exception Failed in
  try
    iteri (fun i c -> if f i c then () else raise Failed) s ;
    true
  with Failed -> false

let forall s ~f = foralli s ~f:(fun _i c -> f c)

let is_sub cur line ~sub =
  cur + length sub <= length line
  && foralli sub ~f:(fun i c -> get line (cur + i) = c)

let is_prefix line ~prefix = is_sub 0 line ~sub:prefix

let sub_from s n = sub s n (length s - n)
