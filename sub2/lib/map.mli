module Make (Ord : Stdlib.Map.OrderedType) : sig
  include Stdlib.Map.S with type key = Ord.t

  val of_list : (key * 'a) list -> 'a t
end
