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


module Self = Options.Self

let plugin_sock = (Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0)

(* let wrapper_port = 8006 *)
(* let maxContLenBufSize = 50 *)
(* let maxPendingRequests = 20 *)
(* let defaultProtocolType = 0 *)
let addr = Unix.inet_addr_of_string "127.0.0.1"



(* Send data in chunks over a socket *)
let send_in_chunks socket data chunk_size =
  let data_len = String.length data in
  let rec send_data offset =
    if offset < data_len then
      let chunk = String.sub data offset (min chunk_size (data_len - offset)) in
      Options.Self.debug ~level:1 "Sending chunck: %s\n%!" chunk;
      let bytes_sent = Unix.send socket (Bytes.of_string chunk) 0 (String.length chunk) [] in
      if bytes_sent = String.length chunk then
        send_data (offset + bytes_sent)  (* Continue sending remaining data *)
      else
        Self.error "Error: only %d bytes sent\n" bytes_sent
  in
  send_data 0  (* Start from offset 0 *)


let send_response plugin_sock response =
  let response_str = Printf.sprintf "Content-Length: %d\r\n\r\n%s" (String.length response) response in
  let chunk_size = 65530 in
  send_in_chunks plugin_sock response_str chunk_size

let send_response_list plugin_sock response_list =
  let response_list = List.rev response_list in
  (* ":::" used to separate diagnostics belonging to different files. *)
  let response = String.concat ":::" response_list in
  send_response plugin_sock response

let is_active_DidSave () = (Options.Did_save.get ())

let get_Handler_args () = 
  let args = Options.Handler_opt.get () in
  if not (String.trim args = "") then (
    let req_info = String.split_on_char ':' (Options.Handler_opt.get ()) in
    let server_port = (Stdlib.int_of_string (List.nth req_info 0)) in
    let wrapper_port = (Stdlib.int_of_string (List.nth req_info 1)) in
    Some (server_port, wrapper_port)
  )
  else None

let get_Wrapper_args () = 
  let wrapper_port = Options.Wrapper_opt.get () in
  if (wrapper_port == 0) then None
  else Some (wrapper_port)

let get_FindDefinition_args () = 
  let args = Options.Find_def.get () in
  if not (String.trim args = "") then
    (
    let req_info = String.split_on_char ':' (Options.Find_def.get ()) in
    let file = (List.nth req_info 0) in
    let line = (Stdlib.int_of_string (List.nth req_info 1)) in
    let ch = (Stdlib.int_of_string (List.nth req_info 2)) in
    Some (Options.Id.get (), file, line, ch)
    )
  else None
let get_FindDeclaration_args () = 
  let args = Options.Find_decl.get () in
  if not (String.trim args = "") then
    (
    let req_info = String.split_on_char ':' (Options.Find_decl.get ()) in
    let file = (List.nth req_info 0) in
    let line = (Stdlib.int_of_string (List.nth req_info 1)) in
    let ch = (Stdlib.int_of_string (List.nth req_info 2)) in
    Some (Options.Id.get (), file, line, ch)
    )
  else None

let get_ComputeProofObligation_args () =
  let args = Options.Show_POVC.get () in
  if not (String.trim args = "") then
    (
    let req_info = String.split_on_char ':' (Options.Show_POVC.get ()) in
    let file = (List.nth req_info 0) in
    let line = (Stdlib.int_of_string (List.nth req_info 1)) in
    let ch = (Stdlib.int_of_string (List.nth req_info 2)) in
    Some (Options.Root_path.get (), Options.Id.get (), file, line, ch)
    )
  else None

let get_ComputeProofObligationID_args () =
    let args = Options.Show_PO.get () in
    if not (String.trim args = "") then
      (
      let goal_id = (Options.Show_PO.get ()) in
      Some (Options.Id.get (), goal_id)
      )
    else None

let get_Prove_args () =
    let args = Options.Prove.get () in
    if not (String.trim args = "") then
      (
      let req_info = String.split_on_char ':' (Options.Prove.get ()) in
      let file = (List.nth req_info 0) in
      let fct = (List.nth req_info 1) in
      let prop = (List.nth req_info 2) in
      Some (Options.Id.get (), file, fct, prop)
      )
    else None

let get_active_option () =
  let active_options = ref [] in
  if is_active_DidSave () then active_options := Lsp_handler.DidSave_feature :: !active_options;
  (match get_FindDefinition_args () with
  | None -> ()
  | Some (id, file, line, ch) -> active_options := Lsp_handler.FindDefinition_feature(id, file, line, ch) :: !active_options
  );
  (match get_FindDeclaration_args () with
  | None -> ()
  | Some (id, file, line, ch) -> active_options := Lsp_handler.FindDeclaration_feature(id, file, line, ch) :: !active_options
  );
  (match get_ComputeProofObligation_args () with
  | None -> ()
  | Some (root_path, id, file, line, ch) -> active_options := Lsp_handler.ComputeProofObligation_feature(root_path, id, file, line, ch) :: !active_options
  );
  (match get_ComputeProofObligationID_args () with
  | None -> ()
  | Some (goal_id) -> active_options := Lsp_handler.ComputeProofObligationID_feature(goal_id) :: !active_options
  );
  (match get_Prove_args () with
  | None -> ()
  | Some (id, file, fct, prop) -> active_options := Lsp_handler.Prove_feature(id, file, fct, prop) :: !active_options
  );
  match !active_options with
  [] -> None
  | [opt] -> Some opt
  | _ -> raise (Failure "Only one option can be specified at once")


let file = ref ""

let diagnostic loc severity msg source = 
  Lsp_types.Diagnostic.create ~range:(Utils.get_lsp_range loc) ~severity:severity ~message:msg ~source:source ()
  
let escape_unicode str = (* todo : write proper function *)
  let regex = Str.regexp {|\\[0-9]+|} in
  Str.global_replace regex "unknown-char" str

let diagnostics_handler (event : Log.event) = 
    let publish_to = ref "" in
    let msg = event.evt_message in
    let _category = match event.evt_category with
      | Some c -> c 
      | None -> "no-category"
    in
    let loc = match event.evt_source with 
      | Some pos -> 
        publish_to := Filepath.normalize (Filepath.Normalized.to_pretty_string pos.pos_path); 
        Utils.real_loc (pos,pos); 
      | None -> (
        publish_to := Filepath.normalize !file;
        Utils.dummyLoc (Filepath.normalize !file))
    in
    let diag_list = DidSave.StringMap.find_opt !publish_to !DidSave.diag_map in
    let diag_list = match diag_list with | None -> [] | Some l -> l in
    match event.evt_kind with 
    | Log.Error ->  
      Options.Self.debug ~level:1 "Error\n%!";
      let diag = diagnostic loc Lsp_types.DiagnosticSeverity.Error (Scanf.unescaped (escape_unicode (String.escaped msg))) event.evt_plugin in
      DidSave.diag_map := DidSave.StringMap.add !publish_to (diag :: diag_list) !DidSave.diag_map
    | Log.Failure ->
      Options.Self.debug ~level:1 "Failure\n%!";
      let diag = diagnostic loc Lsp_types.DiagnosticSeverity.Error (Scanf.unescaped (escape_unicode (String.escaped msg))) event.evt_plugin in
      DidSave.diag_map := DidSave.StringMap.add !publish_to (diag :: diag_list) !DidSave.diag_map
    | Log.Warning ->
      Options.Self.debug ~level:1 "Warning\n%!";
      let diag = diagnostic loc Lsp_types.DiagnosticSeverity.Warning (Scanf.unescaped (escape_unicode (String.escaped msg))) event.evt_plugin in
      DidSave.diag_map := DidSave.StringMap.add !publish_to (diag :: diag_list) !DidSave.diag_map
    | Log.Result -> 
      Options.Self.debug ~level:1 "Result\n%!";
    | Log.Debug -> 
      Options.Self.debug ~level:1 "Debug\n%!";
      let diag = diagnostic loc Lsp_types.DiagnosticSeverity.Information (Scanf.unescaped (escape_unicode (String.escaped msg))) event.evt_plugin in
      DidSave.diag_map := DidSave.StringMap.add !publish_to (diag :: diag_list) !DidSave.diag_map
    | Log.Feedback ->
      if (String.starts_with ~prefix:"Goal" msg) && ((String.ends_with ~suffix:"not tried" msg) || (String.ends_with ~suffix:"trivial" msg)) then ()
      else (
        Options.Self.debug ~level:1 "Feedback\n%!";
        let diag = diagnostic loc Lsp_types.DiagnosticSeverity.Information (Scanf.unescaped (escape_unicode (String.escaped msg))) event.evt_plugin in
        DidSave.diag_map := DidSave.StringMap.add !publish_to (diag :: diag_list) !DidSave.diag_map
      )
  

let set_listerners () =
    Log.add_listener ~plugin:"kernel" (diagnostics_handler);
    Options.Self.debug ~level:1 "kernel listener added\n%!";
    Log.add_listener ~plugin:"wp" (diagnostics_handler);
    Options.Self.debug ~level:1 "wp listener added\n%!";
    Log.add_listener ~plugin:"metacsl" (diagnostics_handler);
    Options.Self.debug ~level:1 "matacsl listener added\n%!";
    Log.add_listener ~plugin:"cc_doc" (diagnostics_handler);
    Options.Self.debug ~level:1 "cc_doc listener added\n%!"

let send_dignostics exn =
  if Options.Enabled.get () then
    (
    Self.debug ~level:1 "Error while processing request : %s, Backtrace : %s\n%!" (Printexc.exn_slot_name exn) (Printexc.get_backtrace ());
    let data = DidSave.StringMap.fold DidSave.publishDiagnostics_notification !DidSave.diag_map [] in
    let data = List.map Json.save_string (data) in
    match get_Wrapper_args() with
      | Some (wrapper_port) ->
        Self.debug ~level:1 "Output results in case of failure !!!";
        Unix.connect plugin_sock (Unix.ADDR_INET(Unix.inet_addr_loopback, wrapper_port));
        ignore (send_response_list plugin_sock data)
      | None -> List.iter (Options.Self.result "JSON result : %s\n%!" ) data
    )


let send_result data =
  match data, (get_Wrapper_args()) with
  | data, Some (wrapper_port) ->
    Self.debug ~level:1 "Sending data to LSP handler ...";
    Unix.connect plugin_sock (Unix.ADDR_INET(Unix.inet_addr_loopback, wrapper_port));
    ignore (send_response_list plugin_sock data)
  | data, None -> List.iter (Options.Self.result "%s\n%!" ) data


let run () = 
  if Options.Enabled.get () then
  (
    match get_Handler_args() with
    | Some (server_port, wrapper_port) -> 
      (
        try Start_server.connect server_port wrapper_port;
        with exn -> Options.Self.debug ~level:1 "There was an error in the server %s:\n Backtrace : %s\n%!" (Printexc.to_string exn) (Printexc.get_backtrace ())
      )
    | None ->
      let framac_share = Utils.file_str Fc_config.datadir in
      Kernel.Share.set (Fc_config.datadir);
      let share = Kernel.Share.get () in
      Filepath.add_symbolic_dir framac_share share;
      let feature = get_active_option () in
      match feature with
      | Some Lsp_handler.DidSave_feature -> let data = List.map Json.save_string (DidSave.handle ()) in Options.Self.feedback ~level:1 "Updated Diagnostics !\n%!"; send_result data
      | Some Lsp_handler.FindDefinition_feature(id, file, line, ch) -> let data = [(Definition.find id file line ch)] in Options.Self.feedback ~level:1 "Find definition attempt done !\n%!"; send_result data
      | Some Lsp_handler.FindDeclaration_feature(id, file, line, ch) -> let data = [(Declaration.find id file line ch)] in Options.Self.feedback ~level:1 "Find declaration attempt done !\n%!"; send_result data
      | Some Lsp_handler.ComputeCIL_feature -> send_result []
      | Some Lsp_handler.ComputeCallGraph_feature _ -> send_result []
      | Some Lsp_handler.ComputeMetrics_feature -> send_result []
      | Some Lsp_handler.ComputeProofObligation_feature(root_path, id, file, line, ch) -> let data = [(ShowPOVC.get_property root_path id file line ch)] in Options.Self.feedback ~level:1 "Find Proof obligation attempt done !\n%!"; send_result data
      | Some Lsp_handler.ComputeProofObligationID_feature(id, goal_id) -> let data = [(ShowPOVC.get_property_from_id id goal_id)] in Options.Self.feedback ~level:1 "Find Proof obligation attempt done !\n%!"; send_result data
      | Some Lsp_handler.Prove_feature(id, file, fct, prop) -> let data = [(ProvePO.get_property_status id file fct prop)] in Options.Self.feedback ~level:1 "Proof attempt done !\n%!"; send_result data
      | None ->  Self.debug ~level:1 "LSP started !!!"
  )

(* let () = Db.Main.extend run *)
let () = 
Frama_c_kernel.Cmdline.run_after_extended_stage set_listerners;
Frama_c_kernel.Cmdline.at_error_exit send_dignostics;
Boot.Main.extend run