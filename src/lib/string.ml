include Stdlib.String

let foralli_from n s ~f =
  let len = length s in
  let rec check i = if i < len then f i (get s i) && check (i + 1) else true in
  check n

let foralli s ~f = foralli_from 0 s ~f
let forall_from n s ~f = foralli_from n s ~f:(fun _i c -> f c)
let forall s ~f = forall_from 0 s ~f

let is_sub cur line ~sub =
  cur + length sub <= length line
  && foralli sub ~f:(fun i c -> Char.equal (get line (cur + i)) c)

let index_sub_from_opt cur line ~sub =
  let len = length line in
  let sub_len = length sub in
  let rec index_sub_from_opt cur =
    if cur + sub_len <= len then
      if foralli sub ~f:(fun i c -> Char.equal (get line (cur + i)) c) then
        Some cur
      else index_sub_from_opt (cur + 1)
    else None
  in
  index_sub_from_opt cur

let index_sub_opt line ~sub = index_sub_from_opt 0 line ~sub
let is_prefix line ~prefix = is_sub 0 line ~sub:prefix
let sub_from s n = sub s n (length s - n)

let split_to_lines s =
  let len = length s in
  let rec split start cur rev =
    if cur < len then
      match get s cur with
      | '\n' ->
          let prev = sub s start (cur - start) in
          split (cur + 1) (cur + 1) (prev :: rev)
      | '\r' when cur + 1 < len && Char.equal (get s (cur + 1)) '\n' ->
          let prev = sub s start (cur - start) in
          split (cur + 2) (cur + 2) (prev :: rev)
      | _ -> split start (cur + 1) rev
    else sub s start (cur - start) :: rev |> List.rev
  in
  split 0 0 []
