let rec apply_span_rules ~unsafe trans_spans state rules =
  match rules with
  | rule :: tl -> (
      let module Rule = (val rule : SpanRule.S) in
      let res =
        if (not unsafe) && not Rule.is_safe then
          (* when it is safe mode, but the rule is unsafe *)
          None
        else Rule.construct (trans_spans ~unsafe) state
      in
      match res with
      | Some _ -> res
      | None -> apply_span_rules ~unsafe trans_spans state tl)
  | [] -> None

let rec trans_spans ~unsafe line =
  let state = { SpanRule.s = line; cur = 0; stack = [] } in
  let len = String.length line in
  let res = Queue.create () in
  let rec aux () =
    if state.cur >= len then (
      List.iter
        (fun stack_elt -> Queue.push (Typ.StackClose stack_elt) res)
        state.stack;
      state.stack <- [])
    else
      let span =
        match
          apply_span_rules ~unsafe trans_spans state
            (AllSpanRules.find line.[state.cur])
        with
        | Some _ as span -> span
        | None -> apply_span_rules ~unsafe trans_spans state AllSpanRules.any
      in
      match span with
      | Some span ->
          Queue.push span res;
          aux ()
      | None ->
          (* NOTE: At last, [SpanRule] in [AllSpanRules] should success always. *)
          assert false
  in
  aux ();
  List.of_seq (Queue.to_seq res)

let trans_spans_from_lines ~unsafe lines =
  RuleBr.construct (trans_spans ~unsafe) lines

let apply_block_rule ~unsafe trans_f line lines rule =
  let module Rule = (val rule : BlockRule.S) in
  let keeps = Queue.create () in
  let construct state left =
    let keep = List.of_seq (Queue.to_seq keeps) in
    Some (Rule.construct trans_f state keep, left)
  in
  let rec apply_block_rule_cont state lines =
    match lines with
    | [] -> if Rule.force_construct then construct state [] else None
    | hd :: tl -> next hd tl (Rule.continue state hd)
  and next line lines res =
    (match res with
    | Go { handle_line = `Keep } | Stop { handle_line = `Keep } ->
        Queue.push line keeps
    | Go { handle_line = `Discard }
    | Stop { handle_line = `Discard | `Left }
    | Die ->
        ());
    match res with
    | Go { state } -> apply_block_rule_cont state lines
    | Stop { state; handle_line } ->
        let left =
          match handle_line with
          | `Keep | `Discard -> lines
          | `Left -> line :: lines
        in
        construct state left
    | Die -> None
  in
  if (not unsafe) && not Rule.is_safe then
    (* when it is safe mode, but the rule is unsafe *)
    None
  else next line lines (Rule.start line)

let rec apply_block_rules ~unsafe trans_f line lines rules =
  match rules with
  | rule :: tl -> (
      match apply_block_rule ~unsafe trans_f line lines rule with
      | Some _ as res -> res
      | None -> apply_block_rules ~unsafe trans_f line lines tl)
  | [] -> None

let block_max_depth = 4

let rec trans_blocks ~depth ~unsafe lines =
  let trans_f =
    {
      BlockRule.trans_spans = trans_spans ~unsafe;
      trans_spans_from_lines = trans_spans_from_lines ~unsafe;
      trans_blocks = trans_blocks ~depth:(depth + 1) ~unsafe;
    }
  in
  let res = Queue.create () in
  let rec aux lines =
    match lines with
    | [] -> ()
    | "" :: tl -> aux tl
    | hd :: tl when depth > block_max_depth -> (
        match apply_block_rule ~unsafe trans_f hd tl (module BlockRuleP) with
        | Some (block, lines) ->
            Queue.push block res;
            aux lines
        | None ->
            (* {!BlockRuleP} should always succeeds. *)
            assert false)
    | hd :: tl -> (
        match
          apply_block_rules ~unsafe trans_f hd tl (AllBlockRules.find hd.[0])
        with
        | Some (block, lines) ->
            Queue.push block res;
            aux lines
        | None -> (
            match apply_block_rules ~unsafe trans_f hd tl AllBlockRules.any with
            | Some (block, lines) ->
                Queue.push block res;
                aux lines
            | None ->
                (* At last, {!BlockRuleP} should always succeeds. *)
                assert false))
  in
  aux lines;
  List.of_seq (Queue.to_seq res)

let run ~unsafe = trans_blocks ~depth:0 ~unsafe
