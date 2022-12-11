(** Possible first character of rules

This information is critical for perfomance. *)

type t = Any | One of char | OneOf of char list

(** Map from character to rules *)
module Map (Elt : sig
  module type Rule

  val first_char : (module Rule) -> t
end) : sig
  type t

  val mem : char -> t -> bool
  val find : char -> t -> (module Elt.Rule) list

  val init : (module Elt.Rule) list -> t * (module Elt.Rule) list
  (** Initialize a map from all rules. The return value is a pair of

   - a map from character to rule list
   - a list of rules that has {!Any} first character *)
end
