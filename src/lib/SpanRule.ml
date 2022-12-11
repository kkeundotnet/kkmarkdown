type state = {
  s : string;
  mutable cur : int;
  mutable stack : Typ.stack_elt list;
}

module type S = sig
  val is_safe : bool
  val first_char : FirstChar.t
  val construct : (string -> Typ.span list) -> state -> Typ.span option
end
