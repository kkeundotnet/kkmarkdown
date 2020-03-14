include Stdlib.Char
module Set = Set.Make (Stdlib.Char)
module Map = Map.Make (Stdlib.Char)

let nums = Set.of_list ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9']

let is_num c = Set.mem c nums

let is_hexa =
  let hexa = Set.union nums (Set.of_list ['a'; 'b'; 'c'; 'd'; 'e'; 'f']) in
  fun c ->
    let c = lowercase_ascii c in
    Set.mem c hexa
