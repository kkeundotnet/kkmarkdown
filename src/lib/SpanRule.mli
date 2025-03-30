(** Span-level rule *)

type state = {
  s : string;  (** string to be parsed *)
  mutable cur : int;  (** current cursor being parsed *)
  mutable stack : Typ.stack_elt list;  (** current stack for {!SpanRuleStack} *)
}

(** Signature for a span-level rule *)
module type S = sig
  val is_safe : bool
  val first_char : FirstChar.t

  val construct : (string -> Typ.span list) -> state -> Typ.span option
  (** Try to apply a rule. The first parameter is the (recursive) function that
      parses a string as a span list. *)
end
