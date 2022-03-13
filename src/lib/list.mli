include module type of Stdlib.List

val pp :
  ?pp_sep:(Format.formatter -> unit) ->
  (Format.formatter -> 'a -> unit) ->
  Format.formatter ->
  'a list ->
  unit

val split_by_first :
  'a list -> f:('a -> bool) -> ('a list * 'a * 'a list) option

val remove_head : 'a list -> f:('a -> bool) -> 'a list
val strip : 'a list -> f:('a -> bool) -> 'a list
val group : 'a list -> f:('a -> bool) -> 'a list list
val append_tailrec : 'a list -> 'a list -> 'a list
