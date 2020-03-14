include Stdlib.List

let pp ?(pp_sep = fun _f -> ()) pp f l =
  let rec pp_list = function
    | [] ->
        ()
    | [x] ->
        pp f x
    | hd :: tl ->
        pp f hd ; pp_sep f ; pp_list tl
  in
  pp_list l

let split_by_first x ~f =
  let rec split_by_first x ~f rev_l =
    match x with
    | [] ->
        None
    | hd :: tl ->
        if f hd then Some (rev_l, hd, tl)
        else split_by_first tl ~f (hd :: rev_l)
  in
  split_by_first x ~f [] |> Option.map (fun (rev_l, e, r) -> (rev rev_l, e, r))

let rec remove_head x ~f =
  match x with hd :: tl when f hd -> remove_head tl ~f | _ -> x
