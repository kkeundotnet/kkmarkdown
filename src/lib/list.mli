include module type of Stdlib.List

val pp :
     ?pp_sep:(Format.formatter -> unit)
  -> (Format.formatter -> 'a -> unit)
  -> Format.formatter
  -> 'a list
  -> unit

val split_by_first :
  'a list -> f:('a -> bool) -> ('a list * 'a * 'a list) option

val remove_head : 'a list -> f:('a -> bool) -> 'a list
