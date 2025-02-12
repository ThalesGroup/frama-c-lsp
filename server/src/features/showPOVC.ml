 (*************************************************************************
 *                                                                        *
 *  This file is part of Frama-C/LSP plugin.                              *
 *                                                                        *
 *  You can redistribute it and/or modify it under the terms of the GNU   *
 *  General Public License as published by the Free Software Foundation,  *
 *  version 3.                                                            *
 *                                                                        *
 *  It is distributed in the hope that it will be useful,                 *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the          *
 *  GNU General Public License for more details.                          *
 *                                                                        *
 *  See the GNU General Public License version 3                          *
 *  for more details (enclosed in the file LICENSE.md                     *
 *                                                                        *
 *************************************************************************)
 
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

let get_property rootPath id file line ch : string =
  let proof_oblgs = ref [] in
  Wp.Wpo.iter_on_goals (fun po ->
    let (start,end_) = Property.location (Wp.Wpo.get_target po) in
    let po_file = Filepath.Normalized.to_pretty_string start.pos_path in
    let line1 = start.pos_lnum in 
    let line2 = end_.pos_lnum in 
    let char1 = start.pos_cnum - start.pos_bol in 
    let char2 = end_.pos_cnum - end_.pos_bol in 
     if (
        (String.equal file (rootPath^"/"^po_file)) 
        && is_position_between (line+1,ch) (line1, char1) (line2,char2)
        && (Wp.WpPropId.is_requires (Wp.Wpo.get_target po))
      ) 
    then 
      proof_oblgs := (Pretty_utils.to_string (Wp.Wpo.pp_goal) po) :: !proof_oblgs
    else if (
          (String.equal file (rootPath^"/"^po_file)) 
          && is_position_between (line+1,ch) (line1, char1) (line2,char2)
        ) 
    then 
      proof_oblgs := (Pretty_utils.to_string (Wp.Wpo.pp_goal) po) :: !proof_oblgs
  );
  let result = Json.of_string (String.concat "\n----------------------------\n" !proof_oblgs) in
  let result_msg =
    match result with 
  | `String "" -> (`String "No proof obligations")
  | _ -> result
  in
  let lsp_message = Lsp_types.ResponseMessage.create ~jsonrpc:"2.0" ~id:(Lsp_types.Int id) ~result:result_msg () in
  let json_message = Lsp_types.ResponseMessage.json_of_t lsp_message in
  Json.save_string json_message





let get_property_from_id id goal_id : string =
    let proof_oblgs = ref [] in
    Wp.Wpo.iter_on_goals (fun po ->
      if (String.equal goal_id (Wp.Wpo.get_gid po)) then
        proof_oblgs := (Pretty_utils.to_string (Wp.Wpo.pp_goal) po) :: !proof_oblgs
    );
    let result = Json.of_string (String.concat "\n----------------------------\n" !proof_oblgs) in
    let result_msg =
      match result with 
    | `String "" -> (`String "No proof obligations")
    | _ -> result
    in
    let lsp_message = Lsp_types.ResponseMessage.create ~jsonrpc:"2.0" ~id:(Lsp_types.Int id) ~result:result_msg () in
    let json_message = Lsp_types.ResponseMessage.json_of_t lsp_message in
    Json.save_string json_message
  
