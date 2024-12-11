

let file = ref ""

let warn_categories = 
  List.map (fun x -> 
    Kernel.wkey_name x
  ) (Kernel.get_all_warn_categories ())

let is_a_warn_category cat : bool = 
  let res = ref false in 
  List.iter (fun x ->
    res := !res || (String.equal cat x)
  ) warn_categories;
  !res

let publishResult id result : Json.json =
  let lsp_message = (Lsp_types.ResponseMessage.create ~jsonrpc:"2.0" ~id:(Lsp_types.Int id) ~result:(`String result) ()) in
  Lsp_types.ResponseMessage.json_of_t lsp_message

let publishDiagnostics_notification filename dlist accumulated_list : Json.json list =
  let dlist = match dlist with
    | [] -> dlist
    | elem :: _l ->
      if (List.length dlist) < 100 then dlist
      else [elem]
  in
  let lsp_notification_params = Lsp_types.PublishDiagnosticsParams.create ~uri:filename ~diagnostics:dlist () in
  let json_notification_params = Lsp_types.PublishDiagnosticsParams.json_of_t lsp_notification_params in
  let lsp_notification = Lsp_types.NotificationMessage.create ~jsonrpc:"2.0" ~method_:"textDocument/publishDiagnostics" ~params:json_notification_params () in
  let json_notification = Lsp_types.NotificationMessage.json_of_t lsp_notification in
  json_notification :: accumulated_list

let clear_diagnostics_no_uri =
  let lsp_notification_params = (Lsp_types.PublishDiagnosticsParams.create ~uri:("") ~diagnostics:([]) ()) in
  let lsp_notification = (Lsp_types.NotificationMessage.create ~jsonrpc:"2.0" ~method_:"textDocument/publishDiagnostics" ~params:(Lsp_types.PublishDiagnosticsParams.json_of_t lsp_notification_params) ()) in
  Lsp_types.NotificationMessage.json_of_t lsp_notification

let clear_diagnostics filename = 
  let lsp_notification_params = (Lsp_types.PublishDiagnosticsParams.create ~uri:(Utils.file_str (Filepath.Normalized.of_string (Filepath.normalize filename))) ~diagnostics:([]) ()) in
  let lsp_notification = Lsp_types.NotificationMessage.create ~jsonrpc:"2.0" ~method_:"textDocument/publishDiagnostics" ~params:(Lsp_types.PublishDiagnosticsParams.json_of_t lsp_notification_params) () in
  Lsp_types.NotificationMessage.json_of_t lsp_notification

let diagnostic loc severity msg source = 
  Lsp_types.Diagnostic.create ~range:(Utils.get_lsp_range loc) ~severity:severity ~message:msg ~source:source ()

let escape_double_quotes str = 
  let regex = Str.regexp {|[\"]|} in
  Str.global_replace regex {|\"|} str

let escape_unicode str = (* todo : write proper function *)
  let regex = Str.regexp {|\\[0-9]+|} in
  Str.global_replace regex "unknown-char" str

let diagnostics_handler (event : Log.event) = 
  (* Lsp.Self.debug ~level:4 "diags handler : nb diags = %d\n%!" (List.length !diag_list); *)
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
  let diag_list = Start_server.StringMap.find_opt !publish_to !Start_server.diag_map in
  let diag_list = match diag_list with
    | None -> []
    | Some l -> l
  in
  if (Utils.contains msg ~suffix:"syntax error" 
    || Utils.contains msg ~suffix:"There were parsing errors in"
    || Utils.contains msg ~suffix:"User Error"
    || Utils.contains msg ~suffix:"invalid user input"
    || Utils.contains msg ~suffix:"Invalid symbol"
    || Utils.contains msg ~suffix:"before or at token"
  ) then
    let diag = diagnostic loc Lsp_types.DiagnosticSeverity.Error (Scanf.unescaped (escape_unicode (String.escaped msg))) event.evt_plugin in
    Start_server.diag_map := Start_server.StringMap.add !publish_to (diag :: diag_list) !Start_server.diag_map
  else
  match event.evt_kind with 
  | Log.Error ->  
    Lsp.Self.debug ~level:4 "Error\n%!";
    let diag = diagnostic loc Lsp_types.DiagnosticSeverity.Error (Scanf.unescaped (escape_unicode (String.escaped msg))) event.evt_plugin in
    Start_server.diag_map := Start_server.StringMap.add !publish_to (diag :: diag_list) !Start_server.diag_map
  | Log.Failure ->
    Lsp.Self.debug ~level:4 "Failure\n%!";
    let diag = diagnostic loc Lsp_types.DiagnosticSeverity.Error (Scanf.unescaped (escape_unicode (String.escaped msg))) event.evt_plugin in
    Start_server.diag_map := Start_server.StringMap.add !publish_to (diag :: diag_list) !Start_server.diag_map
  | Log.Warning -> 
    Lsp.Self.debug ~level:4 "Warning\n%!";
    let diag = diagnostic loc Lsp_types.DiagnosticSeverity.Warning (Scanf.unescaped (escape_unicode (String.escaped msg))) event.evt_plugin in
    Start_server.diag_map := Start_server.StringMap.add !publish_to (diag :: diag_list) !Start_server.diag_map
    (* Lsp.Self.debug ~level:4 "diags handler warning : nb diags = %d\n%!" (List.length !diag_list); *)
  | Log.Result -> 
    Lsp.Self.debug ~level:4 "Result\n%!";
  | Log.Debug -> 
    Lsp.Self.debug ~level:4 "Debug\n%!";
    let diag = diagnostic loc Lsp_types.DiagnosticSeverity.Information (Scanf.unescaped (escape_unicode (String.escaped msg))) event.evt_plugin in
    Start_server.diag_map := Start_server.StringMap.add !publish_to (diag :: diag_list) !Start_server.diag_map
  | Log.Feedback ->
    Lsp.Self.debug ~level:4 "Feedback\n%!"
    (*
    let diag = diagnostic loc Lsp_types.DiagnosticSeverity.Information (Scanf.unescaped (escape_unicode (String.escaped msg))) event.evt_plugin in
    Start_server.diag_map := Start_server.StringMap.add !publish_to (diag :: diag_list) !Start_server.diag_map
    *)
let remove_file_scheme uri =
  let regex = Str.regexp {|file://|} in
  Str.global_replace regex "" uri

let remove_quotes str = 
  let regex = Str.regexp {|[\"]|} in
  Str.global_replace regex "" str

let remove_newline str = 
  let regex = Str.regexp {|.*|} in 
  ignore (Str.search_forward regex str 0);
  Str.matched_string str

let handle filename : Json.json list = 
  Log.add_listener ~plugin:"kernel" (diagnostics_handler);
  Lsp.Self.debug ~level:4 "kernel listener added\n%!";
  Log.add_listener ~plugin:"wp" (diagnostics_handler);
  Lsp.Self.debug ~level:4 "wp listener added\n%!";

  file := filename;
  let filepath = Filepath.Normalized.of_string filename in
  let _file = File.from_filename (filepath) in 
  Kernel.Files.unsafe_set [filepath];
  try
    (* Project.set_current (Project.create "didSave"); *)
    Kernel.Unicode.off ();
    File.init_from_c_files [_file];
    Start_server.StringMap.fold publishDiagnostics_notification !Start_server.diag_map []

  with
  | _exn ->
    (* Lsp.Self.debug ~level:4 "DidSave error :  %s, Backtrace : %s\n%!" (Printexc.exn_slot_name _exn) (Printexc.get_backtrace ()); *)
    Start_server.StringMap.fold publishDiagnostics_notification !Start_server.diag_map []

  

