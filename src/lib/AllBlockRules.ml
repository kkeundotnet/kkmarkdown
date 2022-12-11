(** List of all block rules in descending order *)
let all : (module BlockRule.S) list =
  [
    (module BlockRuleHr);
    (module BlockRuleHeader.Sharp);
    (module BlockRuleHeader.H1);
    (module BlockRuleHeader.H2);
    (module BlockRuleList.Ol);
    (module BlockRuleList.Ul);
    (module BlockRuleCode.Backquote);
    (module BlockRuleCode.Tilde);
    (module BlockRuleCode.UnsafeBackquote);
    (module BlockRuleCode.UnsafeTilde);
    (module BlockRuleCode.Indent);
    (module BlockRuleQuote);
    (module BlockRuleInlineHTML.Div);
    (module BlockRuleInlineHTML.Script);
    (module BlockRuleImg);
    (module BlockRuleP);
  ]

module Map = FirstChar.Map (struct
  module type Rule = BlockRule.S

  let first_char rule =
    let module Rule = (val rule : BlockRule.S) in
    Rule.first_char
end)

let first_char_map, any = Map.init all
let find c = Map.find c first_char_map
