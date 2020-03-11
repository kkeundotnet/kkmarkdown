include Stdlib.List

let split_by_first x ~f =
  let rec split_by_first x ~f rev_r =
    match x with
    | [] ->
        None
    | hd :: tl ->
        if f hd then Some (rev_r, tl) else split_by_first tl ~f (hd :: rev_r)
  in
  split_by_first x ~f [] |> Option.map (fun (rev_r1, r2) -> (rev rev_r1, r2))

let rec remove_head x ~f =
  match x with hd :: tl when f hd -> remove_head tl ~f | _ -> x
