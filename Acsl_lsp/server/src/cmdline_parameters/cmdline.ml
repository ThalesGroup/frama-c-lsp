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

module Prove = Self.String
(struct
  let option_name = "-lsp-prove"
  let help = "send back the proof status"
  let arg_name = "file:fct:prop"
  let default = ""
end)

let wrapper_port_framac = 8006
let maxContLenBufSize = 50
let maxPendingRequests = 20
let defaultProtocolType = 0
let addr = Unix.inet_addr_of_string "127.0.0.1"

let send_response plugin_sock response =
      let response_str = Printf.sprintf "Content-Length: %d\r\n\r\n%s" (String.length response) response in
      let response_bytes = Bytes.of_string response_str in
      let sent = Unix.send plugin_sock (response_bytes) 0 (String.length response_str) [] in
      Self.debug ~level:4 "size : %d content : %s\n%!" sent response_str

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

let get_Prove_args () =
    let args = Prove.get () in
    if not (String.trim args = "") then
      (
      let req_info = String.split_on_char ':' (Prove.get ()) in
      let file = (List.nth req_info 0) in
      let fct = (List.nth req_info 1) in
      let prop = (List.nth req_info 2) in
      Some (Root_path.get (), Id.get (), file, fct, prop)
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
  (match get_Prove_args () with
  | None -> ()
  | Some (root_path, id, file, fct, prop) -> active_options := Lsp_handler.Prove_feature(root_path, id, file, fct, prop) :: !active_options
  );
  match !active_options with
  [] -> None
  | [opt] -> Some opt
  | _ -> raise (Failure "Only one option can be specified at once")



let set_listerners () =
    Log.add_listener ~plugin:"kernel" (DidSave.diagnostics_handler);
    Lsp.Self.debug ~level:4 "kernel listener added\n%!";
    Log.add_listener ~plugin:"wp" (DidSave.diagnostics_handler);
    Lsp.Self.debug ~level:4 "wp listener added\n%!"

let send_dignostics exn =
  if Enabled.get () then
    (
    Self.debug ~level:2 "Error while processing request : %s, Backtrace : %s\n%!" (Printexc.exn_slot_name exn) (Printexc.get_backtrace ());
    let data = Start_server.StringMap.fold DidSave.publishDiagnostics_notification !Start_server.diag_map [] in
    let data = List.map Json.save_string (data) in
    match Cmdline_opt.get () with
      | false ->
        Self.debug ~level:2 "Output results in case of failure !!!";
        Unix.connect plugin_sock (Unix.ADDR_INET(Unix.inet_addr_loopback, wrapper_port_framac));
        ignore (send_response_list plugin_sock data)
      | true -> List.iter (Lsp.Self.result "JSON result : %s\n%!" ) data
    )

let run () = 
  if Enabled.get () then
  (
    if Handler_opt.get () then
      (
        Lsp.Self.debug ~level:3 "Running LSP Handler\n%!";
        try
            Start_server.connect ()
        with exn ->
          Lsp.Self.debug ~level:1 "There was an error in the server %s:\n Backtrace : %s\n%!" (Printexc.to_string exn) (Printexc.get_backtrace ())
      )
    else
    (* try *)
      let framac_share = Utils.file_str Fc_config.datadir in
      Kernel.Share.set (Fc_config.datadir);
      let share = Kernel.Share.get () in
      Filepath.add_symbolic_dir framac_share share;
      let feature = get_active_option () in
      let data = 
      match feature with
      | Some Lsp_handler.DidSave_feature -> List.map Json.save_string (DidSave.handle ())
      (*| Some Lsp_handler.DidClose_feature(file) -> [Json.save_string (DidClose.handle (file))] *)
      | Some Lsp_handler.FindDefinition_feature(id, file, line, ch) -> [(Definition.find id file line ch)]
      | Some Lsp_handler.FindDeclaration_feature(id, file, line, ch) -> [(Declaration.find id file line ch)]
      | Some Lsp_handler.ComputeCIL_feature -> []
      | Some Lsp_handler.ComputeCallGraph_feature -> []
      | Some Lsp_handler.ComputeMetrics_feature -> []
      | Some Lsp_handler.ComputeProofObligation_feature(root_path, id, file, line, ch) -> [(ShowPOVC.get_property root_path id file line ch)]
      | Some Lsp_handler.Prove_feature(root_path, id, file, fct, prop) -> [(ProvePO.get_property_status root_path id file fct prop)]
      | None -> []
      in
      match data, (Cmdline_opt.get ()) with
      | [], _ -> Self.debug ~level:2 "LSP activated !!!";
      | data, false ->
        Self.debug ~level:2 "Output results !!!";
        Unix.connect plugin_sock (Unix.ADDR_INET(Unix.inet_addr_loopback, wrapper_port_framac));
        ignore (send_response_list plugin_sock data)
      | data, true -> List.iter (Lsp.Self.result "JSON result : %s\n%!" ) data
  )

(* let () = Db.Main.extend run *)
let () = 
Frama_c_kernel.Cmdline.run_after_extended_stage set_listerners;
Frama_c_kernel.Cmdline.at_error_exit send_dignostics;
Boot.Main.extend run