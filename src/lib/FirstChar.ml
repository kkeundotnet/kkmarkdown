type t = Any | One of char | OneOf of char list

module Map (Elt : sig
  module type Rule

  val first_char : (module Rule) -> t
end) =
struct
  module M = Map.Make (Char)

  type t = (module Elt.Rule) list M.t

  let add c rule x =
    M.update c (fun rules -> Some (rule :: Option.value rules ~default:[])) x

  let mem = M.mem
  let find c x = Option.value (M.find_opt c x) ~default:[]

  let init all =
    List.fold_right
      (fun rule (map, any) ->
        match Elt.first_char rule with
        | Any -> (map, rule :: any)
        | One c -> (add c rule map, any)
        | OneOf cs ->
            let map = List.fold_left (fun map c -> add c rule map) map cs in
            (map, any))
      all (M.empty, [])
end
