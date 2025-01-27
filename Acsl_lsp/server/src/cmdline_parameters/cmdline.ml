module Self = Lsp.Self

let plugin_sock = (Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0)

let () = Parameter_customize.do_not_projectify ()
module Enabled = Self.False
(struct
  let option_name = "-lsp"
  let help = "when on (off by default), activates lsp support for ACSL/C"
end)

(* module Did_open = Self.String (* filename *)
(struct
  let option_name = "-lsp-did-open"
  let help = "didOpen request"
  let arg_name = "opened file"
  let default = ""

end) *)

let () = Parameter_customize.do_not_projectify ()
module Did_save = Self.False (* filename *)
(struct
  let option_name = "-lsp-did-save"
  let help = "Publish diagnostics each time a file is saved"
end)

(*
module Did_close = Self.String (* filename *)
(struct
  let option_name = "-lsp-did-close"
  let help = "didClose request"
  let arg_name = "file"
  let default = ""
end)
 *)

let () = Parameter_customize.do_not_projectify ()
module Handler_opt = Self.False
(struct
  let option_name = "-lsp-handler"
  let help = "activates handler mode (useful for editors), off by default"
end)

let () = Parameter_customize.do_not_projectify ()
module Cmdline_opt = Self.True
(struct
  let option_name = "-lsp-cmdline"
  let help = "activates command line mode mode (useful for editors), on by default"
end)

module Find_def = Self.String 
(struct
  let option_name = "-lsp-definition"
  let help = "definition request"
  let arg_name = "file:line:character"
  let default = ""
end)

module Find_decl = Self.String 
(struct
  let option_name = "-lsp-declaration"
  let help = "declaration request"
  let arg_name = "file:line:character"
  let default = ""
end)

(* module Find_comp = Self.String 
(struct
  let option_name = "-lsp-completion"
  let help = "completion request"
  let arg_name = "file:line:character"
  let default = ""
end) *)

module Id = Self.Int
(struct
  let option_name = "-lsp-id"
  let help = "id of the request"
  let arg_name = "id"
  let default = 0
end)

module Root_path = Self.String
(struct
  let option_name = "-lsp-root-path"
  let help = "path to the workspace folder"
  let arg_name = "path"
  let default = ""
end)

module Show_POVC = Self.String
(struct
  let option_name = "-lsp-show-povc"
  let help = "send back the povc of the located property"
  let arg_name = "file:line:character"
  let default = ""
end)

module Show_PO = Self.String
(struct
  let option_name = "-lsp-show-po"
  let help = "send back the po of the located property"
  let arg_name = "goal_id"
  let default = ""
end)

module Prove = Self.String
(struct
  let option_name = "-lsp-prove"
  let help = "send back the proof status"
  let arg_name = "fct and prop"
  let default = ""
end)

let wrapper_port_framac = 8006
let maxContLenBufSize = 50
let maxPendingRequests = 20
let defaultProtocolType = 0
let addr = Unix.inet_addr_of_string "127.0.0.1"



(* Send data in chunks over a socket *)
let send_in_chunks socket data chunk_size =
  let data_len = String.length data in
  let rec send_data offset =
    if offset < data_len then
      let chunk = String.sub data offset (min chunk_size (data_len - offset)) in
      Lsp.Self.debug ~level:1 "Sending chunck: %s\n%!" chunk;
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
  (*
  let response_bytes = Bytes.of_string response_str in
  let response_size = (String.length response_str) in
  let sent = Unix.send plugin_sock response_bytes 0 response_size [] in
  Self.debug ~level:2 "size : %d content : %s\n%!" sent response_str
  *)

let send_response_list plugin_sock response_list =
  let response_list = List.rev response_list in
  let response = String.concat ":::" response_list in
  send_response plugin_sock response

let is_active_DidSave () = (Did_save.get ())

let get_FindDefinition_args () = 
  let args = Find_def.get () in
  if not (String.trim args = "") then
    (
    let req_info = String.split_on_char ':' (Find_def.get ()) in
    let file = (List.nth req_info 0) in
    let line = (Stdlib.int_of_string (List.nth req_info 1)) in
    let ch = (Stdlib.int_of_string (List.nth req_info 2)) in
    Some (Id.get (), file, line, ch)
    )
  else None
let get_FindDeclaration_args () = 
  let args = Find_decl.get () in
  if not (String.trim args = "") then
    (
    let req_info = String.split_on_char ':' (Find_decl.get ()) in
    let file = (List.nth req_info 0) in
    let line = (Stdlib.int_of_string (List.nth req_info 1)) in
    let ch = (Stdlib.int_of_string (List.nth req_info 2)) in
    Some (Id.get (), file, line, ch)
    )
  else None

let get_ComputeProofObligation_args () =
  let args = Show_POVC.get () in
  if not (String.trim args = "") then
    (
    let req_info = String.split_on_char ':' (Show_POVC.get ()) in
    let file = (List.nth req_info 0) in
    let line = (Stdlib.int_of_string (List.nth req_info 1)) in
    let ch = (Stdlib.int_of_string (List.nth req_info 2)) in
    Some (Root_path.get (), Id.get (), file, line, ch)
    )
  else None

let get_ComputeProofObligationID_args () =
    let args = Show_PO.get () in
    if not (String.trim args = "") then
      (
      let goal_id = (Show_PO.get ()) in
      Some (Id.get (), goal_id)
      )
    else None

let get_Prove_args () =
    let args = Prove.get () in
    if not (String.trim args = "") then
      (
      let req_info = String.split_on_char ':' (Prove.get ()) in
      let fct = (List.nth req_info 0) in
      let prop = (List.nth req_info 1) in
      Some (Id.get (), fct, prop)
      )
    else None

let get_active_option () =
  let active_options = ref [] in
  if is_active_DidSave () then active_options := Lsp_handler.DidSave_feature :: !active_options;
  (*
  (match get_DidClose_args () with
  | None -> ()
  | Some (file) -> active_options := Lsp_handler.DidClose_feature(file) :: !active_options
  );
  *)
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
  | Some (id, fct, prop) -> active_options := Lsp_handler.Prove_feature(id, fct, prop) :: !active_options
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
    (*
    if (Utils.contains msg ~suffix:"syntax error" 
      || Utils.contains msg ~suffix:"There were parsing errors in"
      || Utils.contains msg ~suffix:"User Error"
      || Utils.contains msg ~suffix:"invalid user input"
      || Utils.contains msg ~suffix:"Invalid symbol"
      || Utils.contains msg ~suffix:"before or at token"
    ) then
      (
      Lsp.Self.debug ~level:1 "Error caught \n%!";
      let diag = diagnostic loc Lsp_types.DiagnosticSeverity.Error (Scanf.unescaped (escape_unicode (String.escaped msg))) event.evt_plugin in
      DidSave.diag_map := DidSave.StringMap.add !publish_to (diag :: diag_list) !DidSave.diag_map
      )
    else
      *)
    match event.evt_kind with 
    | Log.Error ->  
      Lsp.Self.debug ~level:1 "Error\n%!";
      let diag = diagnostic loc Lsp_types.DiagnosticSeverity.Error (Scanf.unescaped (escape_unicode (String.escaped msg))) event.evt_plugin in
      DidSave.diag_map := DidSave.StringMap.add !publish_to (diag :: diag_list) !DidSave.diag_map
    | Log.Failure ->
      Lsp.Self.debug ~level:1 "Failure\n%!";
      let diag = diagnostic loc Lsp_types.DiagnosticSeverity.Error (Scanf.unescaped (escape_unicode (String.escaped msg))) event.evt_plugin in
      DidSave.diag_map := DidSave.StringMap.add !publish_to (diag :: diag_list) !DidSave.diag_map
    | Log.Warning ->
      Lsp.Self.debug ~level:1 "Warning\n%!";
      let diag = diagnostic loc Lsp_types.DiagnosticSeverity.Warning (Scanf.unescaped (escape_unicode (String.escaped msg))) event.evt_plugin in
      DidSave.diag_map := DidSave.StringMap.add !publish_to (diag :: diag_list) !DidSave.diag_map
    | Log.Result -> 
      Lsp.Self.debug ~level:1 "Result\n%!";
    | Log.Debug -> 
      Lsp.Self.debug ~level:1 "Debug\n%!";
      let diag = diagnostic loc Lsp_types.DiagnosticSeverity.Information (Scanf.unescaped (escape_unicode (String.escaped msg))) event.evt_plugin in
      DidSave.diag_map := DidSave.StringMap.add !publish_to (diag :: diag_list) !DidSave.diag_map
    | Log.Feedback ->
      Lsp.Self.debug ~level:1 "Feedback\n%!";
      let diag = diagnostic loc Lsp_types.DiagnosticSeverity.Information (Scanf.unescaped (escape_unicode (String.escaped msg))) event.evt_plugin in
      DidSave.diag_map := DidSave.StringMap.add !publish_to (diag :: diag_list) !DidSave.diag_map
  

let set_listerners () =
    Log.add_listener ~plugin:"kernel" (diagnostics_handler);
    Lsp.Self.debug ~level:1 "kernel listener added\n%!";
    Log.add_listener ~plugin:"wp" (diagnostics_handler);
    Lsp.Self.debug ~level:1 "wp listener added\n%!";
    Log.add_listener ~plugin:"metacsl" (diagnostics_handler);
    Lsp.Self.debug ~level:1 "matacsl listener added\n%!";
    Log.add_listener ~plugin:"cc_doc" (diagnostics_handler);
    Lsp.Self.debug ~level:1 "cc_doc listener added\n%!"

let send_dignostics exn =
  if Enabled.get () then
    (
    Self.debug ~level:1 "Error while processing request : %s, Backtrace : %s\n%!" (Printexc.exn_slot_name exn) (Printexc.get_backtrace ());
    let data = DidSave.StringMap.fold DidSave.publishDiagnostics_notification !DidSave.diag_map [] in
    let data = List.map Json.save_string (data) in
    match Cmdline_opt.get () with
      | false ->
        Self.debug ~level:1 "Output results in case of failure !!!";
        Unix.connect plugin_sock (Unix.ADDR_INET(Unix.inet_addr_loopback, wrapper_port_framac));
        ignore (send_response_list plugin_sock data)
      | true -> List.iter (Lsp.Self.result "JSON result : %s\n%!" ) data
    )


let send_result data =
  match data, (Cmdline_opt.get ()) with
  | data, false ->
    Self.debug ~level:1 "Sending data to LSP handler ...";
    Unix.connect plugin_sock (Unix.ADDR_INET(Unix.inet_addr_loopback, wrapper_port_framac));
    ignore (send_response_list plugin_sock data)
  | data, true -> List.iter (Lsp.Self.result "%s\n%!" ) data


let run () = 
  if Enabled.get () then
  (
    if Handler_opt.get () then
      (
        try Start_server.connect ();
        with exn -> Lsp.Self.debug ~level:1 "There was an error in the server %s:\n Backtrace : %s\n%!" (Printexc.to_string exn) (Printexc.get_backtrace ())
      )
    else
      let framac_share = Utils.file_str Fc_config.datadir in
      Kernel.Share.set (Fc_config.datadir);
      let share = Kernel.Share.get () in
      Filepath.add_symbolic_dir framac_share share;
      let feature = get_active_option () in
      match feature with
      | Some Lsp_handler.DidSave_feature -> let data = List.map Json.save_string (DidSave.handle ()) in Lsp.Self.feedback ~level:1 "Updated Diagnostics !\n%!"; send_result data
      (*| Some Lsp_handler.DidClose_feature(file) -> [Json.save_string (DidClose.handle (file))] *)
      | Some Lsp_handler.FindDefinition_feature(id, file, line, ch) -> let data = [(Definition.find id file line ch)] in Lsp.Self.feedback ~level:1 "Find definition attempt done !\n%!"; send_result data
      | Some Lsp_handler.FindDeclaration_feature(id, file, line, ch) -> let data = [(Declaration.find id file line ch)] in Lsp.Self.feedback ~level:1 "Find declaration attempt done !\n%!"; send_result data
      | Some Lsp_handler.ComputeCIL_feature -> send_result []
      | Some Lsp_handler.ComputeCallGraph_feature _ -> send_result []
      | Some Lsp_handler.ComputeMetrics_feature -> send_result []
      | Some Lsp_handler.ComputeProofObligation_feature(root_path, id, file, line, ch) -> let data = [(ShowPOVC.get_property root_path id file line ch)] in Lsp.Self.feedback ~level:1 "Find Proof obligation attempt done !\n%!"; send_result data
      | Some Lsp_handler.ComputeProofObligationID_feature(id, goal_id) -> let data = [(ShowPOVC.get_property_from_id id goal_id)] in Lsp.Self.feedback ~level:1 "Find Proof obligation attempt done !\n%!"; send_result data
      | Some Lsp_handler.Prove_feature(id, fct, prop) -> let data = [(ProvePO.get_property_status id fct prop)] in Lsp.Self.feedback ~level:1 "Proof attempt done !\n%!"; send_result data
      | None ->  Self.debug ~level:1 "LSP started !!!"
  )

(* let () = Db.Main.extend run *)
let () = 
Frama_c_kernel.Cmdline.run_after_extended_stage set_listerners;
Frama_c_kernel.Cmdline.at_error_exit send_dignostics;
Boot.Main.extend run