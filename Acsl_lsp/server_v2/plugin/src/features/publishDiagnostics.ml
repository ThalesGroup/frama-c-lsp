

let warn_categories = 
  List.map (fun x -> 
    Kernel.wkey_name x
  ) (Kernel.get_all_warn_categories ())

let is_a_warn_category cat : bool = 
  let res = ref false in 
  List.iter (fun x ->
    (* Settings.Self.debug ~level:0 "%s and %s\n%!" cat x; *)
    res := !res || (String.equal cat x)
  ) warn_categories;
  !res

let publishDiagnostics_request dlist filename : Json.json = 
  Types.NotificationMessage.json_of_t (
    Types.NotificationMessage.create
      ~jsonrpc:"2.0"
      ~method_:"textDocument/publishDiagnostics"
      ~params:(Types.PublishDiagnosticsParams.json_of_t 
      (Types.PublishDiagnosticsParams.create
          ~uri:(Utils.file_str (Filepath.Normalized.of_string (Filepath.normalize filename)))
          ~diagnostics:dlist
          ()
      ))
      ()
  )

let publish_to = ref ""
let diag_list = ref []

let clear_diagnostics filename = 
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


let diagnostic loc severity msg = 
  Types.Diagnostic.create 
    ~range:(Utils.get_lsp_range loc)
    ~severity:severity
    ~message:msg
    ()

let diagnostics_handler filename (event : Log.event) = 
    let msg = event.evt_message in
    let _category = match event.evt_category with
      | Some c -> c 
      | None -> "no-category"
    in
    let loc = match event.evt_source with 
      | Some pos -> 
        publish_to := (Filepath.Normalized.to_pretty_string pos.pos_path); 
        Utils.real_loc (pos,pos); 
      | None -> (Utils.dummyLoc filename)
    in
    if (Utils.contains msg ~suffix:"syntax error" 
      || Utils.contains msg ~suffix:"There were parsing errors in"
    ) then 
      begin
        Settings.Self.debug ~level:0 "Kind : Syntax error \n%!";
        diag_list := (diagnostic 
          (loc)
          Types.DiagnosticSeverity.Error 
          msg)::!diag_list
      end
    else
    match event.evt_kind with 
    | Log.Error ->  
      Settings.Self.debug ~level:0 "Kind : Error \n%!";
      diag_list :=  (diagnostic 
          (loc)
          Types.DiagnosticSeverity.Error 
          msg)::!diag_list
    | Log.Failure ->
      Settings.Self.debug ~level:0 "Kind : Failure \n%!";
        diag_list :=  (diagnostic 
          (loc)
          Types.DiagnosticSeverity.Error 
          msg)::!diag_list
    | Log.Warning -> 
      Settings.Self.debug ~level:0 "Kind : Warning \n%!";
      diag_list :=  (diagnostic 
          (loc)
          Types.DiagnosticSeverity.Warning 
          msg)::!diag_list
    | Log.Result -> 
      Settings.Self.debug ~level:0 "Kind : Result \n%!";
      diag_list :=  (diagnostic 
          (loc)
          Types.DiagnosticSeverity.Information 
          msg)::!diag_list
    | Log.Debug -> 
      Settings.Self.debug ~level:0 "Kind : Debug \n%!";
      diag_list := ( (diagnostic 
          (loc)
          Types.DiagnosticSeverity.Information 
          msg))::!diag_list
    | Log.Feedback ->
      Settings.Self.debug ~level:0 "Kind : Feedback \n%!"


let error_event_handler (evt : Log.event) : unit = 
  diagnostics_handler !publish_to evt;
  Settings.Self.debug ~level:0 "diagnostics size : %d\n%!" (List.length (!diag_list))

let () = Log.add_listener ~kind:[Log.Feedback; Log.Warning; Log.Error; Log.Failure] (error_event_handler)