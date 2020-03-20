include Stdlib.String

let foralli s ~f =
  let exception Failed in
  try
    iteri (fun i c -> if f i c then () else raise Failed) s;
    true
  with Failed -> false

let forall_from n s ~f = foralli s ~f:(fun i c -> i < n || f c)

let forall s ~f = forall_from 0 s ~f

let is_sub cur line ~sub =
  cur + length sub <= length line
  && foralli sub ~f:(fun i c -> Char.equal (get line (cur + i)) c)

let rec index_sub_from_opt cur line ~sub =
  if cur + length sub <= length line then
    if foralli sub ~f:(fun i c -> Char.equal (get line (cur + i)) c) then
      Some cur
    else index_sub_from_opt (cur + 1) line ~sub
  else None

let index_sub_opt line ~sub = index_sub_from_opt 0 line ~sub

let is_prefix line ~prefix = is_sub 0 line ~sub:prefix

let sub_from s n = sub s n (length s - n)

let split_to_lines s =
  let rec split start cur rev =
    if cur < length s then
      if is_sub cur s ~sub:"\r\n" then
        let prev = sub s start (cur - start) in
        split (cur + 2) (cur + 2) (prev :: rev)
      else if is_sub cur s ~sub:"\n" then
        let prev = sub s start (cur - start) in
        split (cur + 1) (cur + 1) (prev :: rev)
      else split start (cur + 1) rev
    else sub s start (cur - start) :: rev |> List.rev
  in
  split 0 0 []
