include module type of Stdlib.List

val split_by_first :
  'a list -> f:('a -> bool) -> ('a list * 'a * 'a list) option

val remove_head : 'a list -> f:('a -> bool) -> 'a list
