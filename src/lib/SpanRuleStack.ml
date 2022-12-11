module Make (M : sig
  val first_char : char
  val re : Str.regexp
  val stack_elt : Typ.stack_elt
end) =
struct
  let is_safe = true
  let first_char = FirstChar.One M.first_char

  let construct _ ({ SpanRule.s; cur; stack } as state) : Typ.span option =
    if Str.string_match M.re s cur then (
      state.cur <- Str.match_end ();
      Some
        (match stack with
        | stack_elt :: tl when stack_elt = M.stack_elt ->
            state.stack <- tl;
            StackClose M.stack_elt
        | _ ->
            state.stack <- M.stack_elt :: stack;
            StackOpen M.stack_elt))
    else None
end

module Em = Make (struct
  let first_char = '*'
  let re = Str.regexp "\\*"
  let stack_elt = Typ.Em
end)

module Strong = Make (struct
  let first_char = '*'
  let re = Str.regexp "\\*\\*"
  let stack_elt = Typ.Strong
end)

module EmStrong = Make (struct
  let first_char = '*'
  let re = Str.regexp "\\*\\*\\*"
  let stack_elt = Typ.EmStrong
end)

module Strike = Make (struct
  let first_char = '~'
  let re = Str.regexp "~~"
  let stack_elt = Typ.Strike
end)

module Code = Make (struct
  let first_char = '`'
  let re = Str.regexp "`"
  let stack_elt = Typ.Code
end)
