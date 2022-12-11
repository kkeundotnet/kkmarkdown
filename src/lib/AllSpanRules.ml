(** List of all span rules in descending order, except {!CharsSpan} *)
let all : (module SpanRule.S) list =
  [
    (module SpanRuleEscape);
    (module SpanRuleUnicode.Hex);
    (module SpanRuleUnicode.Dec);
    (module SpanRuleStack.EmStrong);
    (module SpanRuleStack.Strong);
    (module SpanRuleStack.Em);
    (module SpanRuleStack.Strike);
    (module SpanRuleStack.Code);
    (module SpanRuleA.Automatic);
    (module SpanRuleA.UnsafeNormal);
  ]

module Map = FirstChar.Map (struct
  module type Rule = SpanRule.S

  let first_char rule =
    let module Rule = (val rule : SpanRule.S) in
    Rule.first_char
end)

let first_char_map, any = Map.init all
let find c = Map.find c first_char_map

module SpanRuleChars = struct
  let is_safe = true
  let first_char = FirstChar.Any

  let construct _ ({ SpanRule.s; cur } as state) =
    let len = String.length s in

    let next_cur =
      let exception Done of int in
      try
        for i = cur + 1 to len - 1 do
          if Map.mem s.[i] first_char_map then raise (Done i)
        done;
        len
      with Done i -> i
    in
    let res = Typ.CharsSpan (String.sub s cur (next_cur - cur)) in
    state.cur <- next_cur;
    Some res
end

let any = any @ [ (module SpanRuleChars : SpanRule.S) ]
