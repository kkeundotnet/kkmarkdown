include Stdlib.Char

let is_num c =
  match c with
  | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> true
  | _ -> false

let is_hexa c =
  is_num c
  ||
  match lowercase_ascii c with
  | 'a' | 'b' | 'c' | 'd' | 'e' | 'f' -> true
  | _ -> false
