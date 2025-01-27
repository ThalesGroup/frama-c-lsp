

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



let get_property_status id file fct prop: string =
  let root_dir = ".frama-c" in
  if not (Sys.file_exists root_dir) then Unix.mkdir root_dir 0o755;
  let verdict_msg = ref [] in
  Wp.Wpo.iter_on_goals (fun po -> 
    Lsp.Self.debug ~level:1 "gid:%s label:%s done!\n%!" (Wp.Wpo.get_gid po) (Wp.Wpo.get_label po);
    (* let prover_result_list = (Wp.Wpo.get_results po) in
    let prover_result_list = (List.map (fun (p, r) -> (Printf.sprintf "%s==>%s" (Pretty_utils.to_string Wp.VCS.pp_prover p) (Pretty_utils.to_string Wp.VCS.pp_result r))) prover_result_list) in
    let prover_results = String.concat " " prover_result_list in *)
    let stats = Wp.ProofEngine.consolidated po in
    let prover_results = (Pretty_utils.to_string Wp.Stats.pretty stats) in
    let proof_status, property = (Wp.Wpo.get_proof po) in
    let function_name = match Wp.Wpo.get_index po with
    | Axiomatic _a -> "@axiomatic"
    | Function (kf, _) -> Ast_info.Function.get_name kf.fundec
    (*| Function (kf, s) -> Pretty_utils.to_string (fun x y -> Wp.Wpo.pp_function x y s) kf *)
    in
    (* let property_name = Property.Names.get_prop_name_id property in *)
    (* let property_name = Property.Names.get_prop_basename property in *)
    let property_name = match Property.get_names property with 
    [] -> "No labels given"
    | l :: _ -> l
    in
    Lsp.Self.debug ~level:1 "function:%s label:%s done!\n%!" function_name property_name;
    let script_file = Pretty_utils.to_string Wp.ProofSession.pp_file (Wp.ProofSession.filename ~force:false po) in
    let position = match Property.source property with
    | None -> ""
    | Some position -> (Pretty_utils.to_string Filepath.pp_pos position) 
    in 
    let goal_id = Wp.Wpo.get_gid po in
    let po_file = Printf.sprintf "%s/%s.txt" root_dir goal_id in
    (* let po_str = (Pretty_utils.to_string (Wp.Wpo.pp_goal) po) in *)
    (* let po_str = (Pretty_utils.to_string (Wp.Wpo.pp_goal_flow) po) in *)
    let po_tree = Wp.ProofEngine.proof ~main:po in
    let po_node = Wp.ProofEngine.root po_tree in
    let rec get_leaves acc node =
      match Wp.ProofEngine.subgoals node with
      | [] -> acc @ [node]
      | l -> (List.fold_left get_leaves acc l)
    in
    let po_node_list = get_leaves [] po_node in
    (* let po_node_list = Wp.ProofEngine.children po_node in *)
    let get_tactics po_node =
      let po_path = Wp.ProofEngine.path po_node in
      let po_tactics = List.map (fun x -> match Wp.ProofEngine.tactic_label x with None -> "." | Some s -> s) po_path in
      String.concat "\n" po_tactics
    in
    let po_subgoals = List.map (fun x -> (Wp.ProofEngine.goal x, get_tactics x)) po_node_list in
    let po_str = String.concat "\n" (List.map (fun (x,y) -> ((Pretty_utils.to_string Wp.Wpo.pp_goal_flow) x) ^ "\n" ^ y) po_subgoals) in

    let output_channel = open_out po_file in
    output_string output_channel po_str;
    close_out output_channel;
    let _property_id = (Property.Names.get_prop_name_id property) in
    match proof_status with
    | `Passed -> verdict_msg := `String (Printf.sprintf "passed:%s:%s:%s:%s:%s:%s\n%!" goal_id position prover_results script_file function_name property_name) :: !verdict_msg
    | `Failed -> verdict_msg := `String (Printf.sprintf "failed:%s:%s:%s:%s:%s:%s\n%!" goal_id position prover_results script_file function_name property_name) :: !verdict_msg
    | `Unknown -> verdict_msg := `String (Printf.sprintf "unknown:%s:%s:%s:%s:%s:%s\n%!" goal_id position prover_results script_file function_name property_name) :: !verdict_msg
    );
  let result_msg = (`List !verdict_msg) in
  let result_msg = (`List [`String file; `String fct; `String prop; result_msg]) in
  let lsp_message = Lsp_types.ResponseMessage.create ~jsonrpc:"2.0" ~id:(Lsp_types.Int id) ~result:result_msg () in
  let json_message = Lsp_types.ResponseMessage.json_of_t lsp_message in
  Json.save_string json_message

