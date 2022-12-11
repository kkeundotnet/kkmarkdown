(** Block-level rule *)

type 'state res =
  | Go of { state : 'state; handle_line : [ `Keep | `Discard ] }
  | Stop of { state : 'state; handle_line : [ `Keep | `Discard | `Left ] }
  | Die

type trans_f = {
  trans_spans : string -> Typ.span list;
  trans_spans_from_lines : string list -> Typ.span list;
  trans_blocks : string list -> Typ.block list;
}

(** Signature for a block-level rule *)
module type S = sig
  val is_safe : bool
  val first_char : FirstChar.t

  type state

  val start : string -> state res
  val continue : state -> string -> state res
  val construct : trans_f -> state -> string list -> Typ.block

  val force_construct : bool
  (** True if it should construct a block when no further input line is given *)
end
