

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
  let verdict_msg = ref [] in
  Wp.Wpo.iter_on_goals (fun po -> 
    Lsp.Self.debug ~level:2 "gid:%s label:%s done!\n%!" (Wp.Wpo.get_gid po) (Wp.Wpo.get_label po);
    (* let prover_result_list = (Wp.Wpo.get_results po) in
    let prover_result_list = (List.map (fun (p, r) -> (Printf.sprintf "%s==>%s" (Pretty_utils.to_string Wp.VCS.pp_prover p) (Pretty_utils.to_string Wp.VCS.pp_result r))) prover_result_list) in
    let prover_results = String.concat " " prover_result_list in *)
    let stats = Wp.ProofEngine.consolidated po in
    let prover_results = (Pretty_utils.to_string Wp.Stats.pretty stats) in
    let proof_status, property = (Wp.Wpo.get_proof po) in
    let script_file = Pretty_utils.to_string Wp.ProofSession.pp_script_for po in
    let position = match Property.source property with
    | None -> ""
    | Some position -> (Pretty_utils.to_string Filepath.pp_pos position) 
    in 
    match proof_status with
    | `Passed -> verdict_msg := `String (Printf.sprintf "passed:%s:%s:%s:%s\n%!" (Property.Names.get_prop_basename property) position prover_results script_file) :: !verdict_msg
    | `Failed -> verdict_msg := `String (Printf.sprintf "failed:%s:%s:%s:%s\n%!" (Property.Names.get_prop_basename property) position prover_results script_file) :: !verdict_msg
    | `Unknown -> verdict_msg := `String (Printf.sprintf "unknown:%s:%s:%s:%s\n%!" (Property.Names.get_prop_basename property) position prover_results script_file) :: !verdict_msg
    );
  let result_msg = (`List !verdict_msg) in
  let lsp_message = Lsp_types.ResponseMessage.create ~jsonrpc:"2.0" ~id:(Lsp_types.Int id) ~result:result_msg () in
  let json_message = Lsp_types.ResponseMessage.json_of_t lsp_message in
  Json.save_string json_message

