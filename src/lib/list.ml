include Stdlib.List

let pp ?(pp_sep = fun _f -> ()) pp f l =
  let rec pp_list = function
    | [] -> ()
    | [ x ] -> pp f x
    | hd :: tl ->
        pp f hd;
        pp_sep f;
        pp_list tl
  in
  pp_list l

let split_by_first x ~f =
  let rec split_by_first x ~f rev_l =
    match x with
    | [] -> None
    | hd :: tl ->
        if f hd then Some (rev_l, hd, tl) else split_by_first tl ~f (hd :: rev_l)
  in
  split_by_first x ~f [] |> Option.map (fun (rev_l, e, r) -> (rev rev_l, e, r))

let rec remove_head x ~f =
  match x with hd :: tl when f hd -> remove_head tl ~f | _ -> x

let strip x ~f =
  let rec strip_head_rev rev = function
    | [] -> rev
    | hd :: tl ->
        if f hd then strip_head_rev rev tl
        else Stdlib.List.rev_append tl (hd :: rev)
  in
  strip_head_rev [] x |> strip_head_rev []

let group =
  let new_group e = Some [ e ] in
  let cons_group e = function None -> new_group e | Some g -> Some (e :: g) in
  let cons_groups g_opt gs =
    match g_opt with None -> gs | Some g -> g :: gs
  in
  let rec group ~f l rev_g rev_gs =
    match l with
    | [] -> rev (cons_groups (Option.map rev rev_g) rev_gs)
    | hd :: tl when f hd ->
        group ~f tl (new_group hd) (cons_groups (Option.map rev rev_g) rev_gs)
    | hd :: tl -> group ~f tl (cons_group hd rev_g) rev_gs
  in
  fun x ~f -> group ~f x None []

let append_tailrec x y = rev_append (rev x) y
