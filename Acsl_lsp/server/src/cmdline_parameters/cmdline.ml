module Self = Lsp.Self

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

module Did_save = Self.String (* filename *)
(struct
  let option_name = "-lsp-did-save"
  let help = "did save. provide source file here instead of providing it to frama-c directly."
  let arg_name = "file"
  let default = ""

end)

module Did_close = Self.String (* filename *)
(struct
  let option_name = "-lsp-did-close"
  let help = "didClose request"
  let arg_name = "file"
  let default = ""

end)

module Handler_opt = Self.False
(struct
  let option_name = "-lsp-handler"
  let help = "activates handler mode (useful for editors), off by default"
end)

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

module Display_CIL = Self.String
(struct
  let option_name = "-lsp-display-cil"
  let help = "send back the ast of the current file to editor"
  let arg_name = "filename without extension"
  let default = ""
end)

module Compute_CG = Self.String
(struct
  let option_name = "-lsp-compute-cg"
  let help = "send back the callgraph of the current file to the editor"
  let arg_name = "filename without extension"
  let default = ""
end)

module Show_POVC = Self.String
(struct
  let option_name = "-lsp-show-povc"
  let help = "send back the povc of the located property"
  let arg_name = "file:line:character"
  let default = ""
end)

module Show_metrics = Self.String
(struct
  let option_name = "-lsp-metrics"
  let help = "send metrics"
  let arg_name = "filename without extension"
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
 
type lsp_feature = 
  | DidSave_feature
  | DidClose_feature
  | FindDefinition_feature
  | FindDeclaration_feature
  | ComputeCIL_feature
  | ComputeCallGraph_feature
  | ComputeMetrics_feature
  | ComputeProofObligation_feature

let is_active_DidSave () = not (String.equal (Did_save.get ()) "")
let is_active_DidClose () = not (String.equal (Did_close.get ()) "")
let is_active_FindDefinition () = not (String.equal (Find_def.get ()) "")
let is_active_FindDeclaration () = not (String.equal (Find_decl.get ()) "")
let is_active_ComputeCIL () = not (String.equal (Display_CIL.get ()) "")
let is_active_ComputeCallGraph () = not (String.equal (Compute_CG.get ()) "")
let is_active_ComputeMetrics () = not (String.equal (Show_metrics.get ()) "")
let is_active_ComputeProofObligation () = not (String.equal (Show_POVC.get ()) "")


let get_active_option () =
  let active_options = ref [] in
  if is_active_DidSave () then active_options := DidSave_feature :: !active_options;
  if is_active_DidClose () then active_options := DidClose_feature :: !active_options;
  if is_active_FindDefinition () then active_options := FindDefinition_feature :: !active_options;
  if is_active_FindDeclaration () then active_options := FindDeclaration_feature :: !active_options;
  if is_active_ComputeCIL () then active_options := ComputeCIL_feature :: !active_options;
  if is_active_ComputeCallGraph () then active_options := ComputeCallGraph_feature :: !active_options;
  if is_active_ComputeMetrics () then active_options := ComputeMetrics_feature :: !active_options;
  if is_active_ComputeProofObligation () then active_options := ComputeProofObligation_feature :: !active_options;
  match !active_options with
  [] -> raise (Failure "No option specified")
  | [opt] -> opt
  | _ -> raise (Failure "Only one option can be specified at once")


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
    let plugin_sock = (Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0) in
    try 
      let framac_share = Utils.file_str Fc_config.datadir in
      Kernel.Share.set (Fc_config.datadir);
      let share = Kernel.Share.get () in
      Filepath.add_symbolic_dir framac_share share;
      let feature = get_active_option () in
      let data = 
      match feature with
      | DidSave_feature -> 
        let data = List.map Json.save_string (DidSave.handle (Did_save.get ())) in
        data
      | DidClose_feature ->
        let data = Json.save_string (DidClose.handle (Did_close.get ())) in
        data :: []
      | FindDefinition_feature ->
        let req_info = String.split_on_char ':' (Find_def.get ()) in 
        let int_id = (Id.get ()) in 
        let data = Json.save_string (Definition.find int_id (List.nth req_info 0) (Stdlib.int_of_string (List.nth req_info 1)) (Stdlib.int_of_string (List.nth req_info 2))) in
        data :: []
      | FindDeclaration_feature ->
        let req_info = String.split_on_char ':' (Find_decl.get ()) in 
        let int_id = (Id.get ()) in 
        let data = Json.save_string (Declaration.find int_id (List.nth req_info 0) (Stdlib.int_of_string (List.nth req_info 1)) (Stdlib.int_of_string (List.nth req_info 2))) in
        data :: []
      | ComputeCIL_feature -> 
        let lsp_message = Lsp_types.ShowMessageParams.create ~type_: Lsp_types.MessageType.Info ~message: (Printf.sprintf "Calculated CIL successfully, file generated : %s_fc.c" (Display_CIL.get ())) () in
        let lsp_notification = Lsp_types.NotificationMessage.create ~jsonrpc:"2.0" ~method_:"window/showMessage" ~params: (Lsp_types.ShowMessageParams.json_of_t lsp_message) () in
        let data = Json.save_string (Lsp_types.NotificationMessage.json_of_t lsp_notification) in
        data :: []
      | ComputeCallGraph_feature ->
        ignore (Sys.command ("dot -Tpdf "^(Compute_CG.get ())^".dot -o "^(Compute_CG.get ())^".pdf"));
        Lsp.Self.debug ~level:2 ("Generated %s.dot and %s.pdf files") (Compute_CG.get ()) (Compute_CG.get ());
        let lsp_message = Lsp_types.ShowMessageParams.create ~type_: Lsp_types.MessageType.Info ~message: ("Computed callgraph successfully, files generated : "^(Compute_CG.get ())^".dot and "^(Compute_CG.get ())^".pdf") () in
        let lsp_notification = Lsp_types.NotificationMessage.create ~jsonrpc:"2.0" ~method_:"window/showMessage" ~params: (Lsp_types.ShowMessageParams.json_of_t lsp_message) () in
        let data = Json.save_string (Lsp_types.NotificationMessage.json_of_t lsp_notification) in
        data :: []
      | ComputeMetrics_feature ->
        let lsp_message = Lsp_types.ShowMessageParams.create ~type_: Lsp_types.MessageType.Info ~message: (Printf.sprintf "Calculated metrics successfully, file generated : %s.txt" (Show_metrics.get ())) () in
        let lsp_notification = Lsp_types.NotificationMessage.create ~jsonrpc:"2.0" ~method_:"window/showMessage" ~params: (Lsp_types.ShowMessageParams.json_of_t lsp_message) () in
        let data = Json.save_string (Lsp_types.NotificationMessage.json_of_t lsp_notification) in
        data :: []
      | ComputeProofObligation_feature ->
          let req_info = String.split_on_char ':' (Show_POVC.get ()) in 
          let id = Id.get () in 
          let file = List.nth req_info 0 in 
          let line = Stdlib.int_of_string (List.nth req_info 1) in
          let ch = Stdlib.int_of_string (List.nth req_info 2) in
          let result = ShowPOVC.get_property (Root_path.get ()) file line ch in
          let result_msg =
            match result with 
          | `String "" -> (`String "No proof obligations")
          | _ -> result
          in
          let lsp_message = Lsp_types.ResponseMessage.create ~jsonrpc:"2.0" ~id: (Lsp_types.Int id) ~result:result_msg () in
          let data = Json.save_string (Lsp_types.ResponseMessage.json_of_t lsp_message) in
          data :: []
        (* if not (String.equal (Acsl_wp.get ()) "") then 
          (
            (* let id = Id.get () in  *)
            let file = Acsl_wp.get () in 
            let data = Json.save_string (AcslWp.handle file) in
  
            match Cmdline_opt.get () with
            | false -> Unix.connect plugin_sock (Unix.ADDR_INET(Unix.inet_addr_loopback, wrapper_port_framac));
              ignore (send_response plugin_sock data)
            | true -> ignore (Lsp.Self.result "JSON result : %s\n%!" data) ;
          );  *)        

      (* if not (String.equal (Find_comp.get ()) "") then
        (
          let req_info = String.split_on_char ':' (Find_comp.get ()) in 
          let int_id = (Id.get ()) in 
          let data = Json.save_string (Completion.completion_items int_id (List.nth req_info 0) (Stdlib.int_of_string (List.nth req_info 1)) (Stdlib.int_of_string (List.nth req_info 2))) in
          match Cmdline_opt.get () with
          | false -> Unix.connect plugin_sock (Unix.ADDR_INET(Unix.inet_addr_loopback, wrapper_port_framac));
            ignore (send_response plugin_sock data)
          | true -> ignore (Lsp.Self.result "JSON result : %s\n%!" data) ;
        ); *)
      in
      match Cmdline_opt.get () with
      | false -> Unix.connect plugin_sock (Unix.ADDR_INET(Unix.inet_addr_loopback, wrapper_port_framac));
        ignore (send_response_list plugin_sock data)
      | true -> List.iter (Lsp.Self.result "JSON result : %s\n%!" ) data
  
  with exn ->
    Self.debug ~level:2 "Error while processing request : %s, Backtrace : %s\n%!" (Printexc.exn_slot_name exn) (Printexc.get_backtrace ());
    (* Unix.connect plugin_sock (Unix.ADDR_INET(Unix.inet_addr_loopback, wrapper_port_framac)); *)
      let lsp_error_message = Lsp_types.ResponseError.create ~code:(-32603) ~message:(Printexc.get_backtrace ()) () in
      let lsp_message = Lsp_types.ResponseMessage.create ~jsonrpc:"2.0" ~id:(Lsp_types.Str "frama_c_error") ~error: lsp_error_message () in 
      let data = Json.save_string (Lsp_types.ResponseMessage.json_of_t lsp_message) in
      let data = data :: [] in
      match Cmdline_opt.get () with
      | false -> Unix.connect plugin_sock (Unix.ADDR_INET(Unix.inet_addr_loopback, wrapper_port_framac));
        ignore (send_response_list plugin_sock data)
      | true -> List.iter (Lsp.Self.result "JSON result : %s\n%!" ) data
  )


(* let () = Db.Main.extend run *)
let () = Boot.Main.extend run