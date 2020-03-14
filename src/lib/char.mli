include module type of Stdlib.Char

val is_num : char -> bool

val is_hexa : char -> bool

module Set : module type of Set.Make (Stdlib.Char)

module Map : module type of Map.Make (Stdlib.Char)
