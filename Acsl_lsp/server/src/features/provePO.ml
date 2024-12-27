

let get_all () : Json.json = 
  let proof_oblgs = ref [] in
  Wp.Wpo.iter_on_goals (fun po ->
    proof_oblgs := (Pretty_utils.to_string (Wp.Wpo.pp_goal) po) :: !proof_oblgs
  );
  Json.of_string (String.concat "\n----------------------------\n" !proof_oblgs)

let is_position_between (line_check, char_check) (line1, char1) (line2, char2) =
  let (line1, char1, line2, char2) =
    if (line1, char1) > (line2, char2) then
      (line2, char2, line1, char1)
    else
      (line1, char1, line2, char2)
  in

  if line1 < line_check && line_check < line2 then
    true
  else if line_check = line1 then
    char1 <= char_check
  else if line_check = line2 then
    char_check <= char2
  else if line1 = line_check && line_check = line2 then
    char1 <= char_check && char_check <= char2
  else
    false



let get_property_status _rootPath id _file _fct _prop : string =
  Wp.Wpo.iter_on_goals (fun po -> 
    Lsp.Self.debug ~level:2 "gid:%s label:%s done!\n%!" (Wp.Wpo.get_gid po) (Wp.Wpo.get_label po);
    let proof_status, property = (Wp.Wpo.get_proof po) in
    match proof_status with
    | `Passed -> Lsp.Self.debug ~level:2 "passed:%s\n%!" (Property.Names.get_prop_basename property)
    | `Failed -> Lsp.Self.debug ~level:2 "failed:%s\n%!" (Property.Names.get_prop_basename property)
    | `Unknown -> Lsp.Self.debug ~level:2 "unknown:%s\n%!" (Property.Names.get_prop_basename property)
    );
  let result_msg = (`String "Proof not implemented yet !") in
  let lsp_message = Lsp_types.ResponseMessage.create ~jsonrpc:"2.0" ~id:(Lsp_types.Int id) ~result:result_msg () in
  let json_message = Lsp_types.ResponseMessage.json_of_t lsp_message in
  Json.save_string json_message

