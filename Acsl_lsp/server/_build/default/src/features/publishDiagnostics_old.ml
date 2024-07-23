let warn_categories = 
  List.map (fun x -> 
    Kernel.wkey_name x
  ) (Kernel.get_all_warn_categories ())

let is_a_warn_category cat : bool = 
  let res = ref false in 
  List.iter (fun x ->
    (* Settings.Self.debug ~level:1 "%s and %s\n%!" cat x; *)
    res := !res || (String.equal cat x)
  ) warn_categories;
  !res

let clear_diagnostics filename = 
  Json.save_string (
    Types.NotificationMessage.json_of_t (
      Types.NotificationMessage.create
        ~jsonrpc:"2.0"
        ~method_:"textDocument/publishDiagnostics"
        ~params:(Types.PublishDiagnosticsParams.json_of_t 
        (Types.PublishDiagnosticsParams.create
            ~uri:(Utils.file_str (Filepath.Normalized.of_string (Filepath.normalize filename)))
            ~diagnostics:(
              []
            )
            ()
        ))
        ()
    )
  )

let diagnostic filename loc severity msg = 
  Json.save_string (
    Types.NotificationMessage.json_of_t (
      Types.NotificationMessage.create
        ~jsonrpc:"2.0"
        ~method_:"textDocument/publishDiagnostics"
        ~params:(Types.PublishDiagnosticsParams.json_of_t 
        (Types.PublishDiagnosticsParams.create
            ~uri:(Utils.file_str (Filepath.Normalized.of_string (Filepath.normalize filename)))
            ~diagnostics:(
              [
                Types.Diagnostic.create 
                  ~range:(Utils.get_lsp_range loc)
                  ~severity:severity
                  ~message:msg
                  ()
              ]
            )
            ()
        ))
        ()
    )
  ) 

let diagnostics_handler filename (event : Log.event) = (* todo : use workspace uri if no specified file for the diagnostic *)
    let msg = event.evt_message in
    let _category = match event.evt_category with
      | Some c -> c 
      | None -> "no-category"
    in
    let loc = match event.evt_source with 
      | Some pos -> Utils.real_loc (pos,pos) 
      | None -> (Utils.dummyLoc filename)
    in
    if (Utils.contains msg ~suffix:"syntax error" 
      || Utils.contains msg ~suffix:"There were parsing errors in"
      (* || Utils.contains msg ~suffix:"Can't preprocess annotation: " *)
    ) then 
      begin
        Settings.Self.debug ~level:1 "Kind : Syntax error \n%!";
        diagnostic 
          filename 
          (loc)
          Types.DiagnosticSeverity.Error 
          msg
      end
    else
    match event.evt_kind with 
    | Log.Error ->  
      Settings.Self.debug ~level:1 "Kind : Error \n%!";
        diagnostic 
          filename 
          (loc)
          Types.DiagnosticSeverity.Error 
          msg
    | Log.Failure ->
      Settings.Self.debug ~level:1 "Kind : Failure \n%!";
      if (Utils.contains event.evt_message ~suffix:"Errorloc.finishParsing called while lexbuf is empty") then
        begin 
          clear_diagnostics filename; 
        end
      else if (Utils.contains event.evt_message ~suffix:"[Errorloc.startParsing]") then 
        begin 
          clear_diagnostics filename;
        end
      else
        diagnostic 
          filename 
          (loc)
          Types.DiagnosticSeverity.Error 
          msg
    | Log.Warning -> 
      Settings.Self.debug ~level:1 "Kind : Warning \n%!";
      diagnostic 
          filename 
          (loc)
          Types.DiagnosticSeverity.Warning 
          msg
    | Log.Result -> 
      Settings.Self.debug ~level:1 "Kind : Result \n%!";
      diagnostic 
          filename 
          (loc)
          Types.DiagnosticSeverity.Information 
          msg
    | Log.Debug -> 
      Settings.Self.debug ~level:1 "Kind : Debug \n%!";
      diagnostic 
          filename 
          (loc)
          Types.DiagnosticSeverity.Information 
          msg
    | Log.Feedback ->
      Settings.Self.debug ~level:1 "Kind : Feedback \n%!";
      clear_diagnostics filename




let error_event_handler sock (evt : Log.event) : unit = 
  (* let file = Project.get_name (Project.current ()) in *)
  let file = !DidSave.filename in
  (* let project = Project.get_name (Project.current ()) in *)
  (* if (String.equal (project) "global_ast") then *)
  (* let file = "project" in *)
  Utils.send_request sock (diagnostics_handler file evt);

