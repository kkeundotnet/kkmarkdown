include Stdlib.Char

let is_num c =
  c = '0' || c = '1' || c = '2' || c = '3' || c = '4' || c = '5' || c = '6'
  || c = '7' || c = '8' || c = '9'

let is_hexa c =
  let c = lowercase_ascii c in
  is_num c || c = 'a' || c = 'b' || c = 'c' || c = 'd' || c = 'e' || c = 'f'

module Set = Set.Make (Stdlib.Char)
module Map = Map.Make (Stdlib.Char)
