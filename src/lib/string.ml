include Stdlib.String

let foralli s ~f =
  let exception Failed in
  try
    iteri (fun i c -> if f i c then () else raise Failed) s ;
    true
  with Failed -> false

let forall_from n s ~f = foralli s ~f:(fun i c -> i < n || f c)

let forall s ~f = forall_from 0 s ~f

let is_sub cur line ~sub =
  cur + length sub <= length line
  && foralli sub ~f:(fun i c -> get line (cur + i) = c)

let is_prefix line ~prefix = is_sub 0 line ~sub:prefix

let sub_from s n = sub s n (length s - n)
