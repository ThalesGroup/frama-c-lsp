

let file = ref ""
let diag_list = ref []

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
  Lsp_types.ResponseMessage.json_of_t(
    Lsp_types.ResponseMessage.create 
    ~jsonrpc:"2.0"
    ~id:(Lsp_types.Int id)
    ~result:(`String result)
      ()
    )

let publishDiagnostics_notification dlist filename: Json.json = 
    Lsp_types.NotificationMessage.json_of_t (
      Lsp_types.NotificationMessage.create
        ~jsonrpc:"2.0"
        ~method_:"textDocument/publishDiagnostics"
        ~params:(Lsp_types.PublishDiagnosticsParams.json_of_t 
        (Lsp_types.PublishDiagnosticsParams.create
            ~uri:(filename)
            ~diagnostics:dlist
            ()
        ))
        ()
    )

let clear_diagnostics_no_uri = 
  Lsp_types.NotificationMessage.json_of_t (
    Lsp_types.NotificationMessage.create
      ~jsonrpc:"2.0"
      ~method_:"textDocument/publishDiagnostics"
      ~params:(Lsp_types.PublishDiagnosticsParams.json_of_t 
      (Lsp_types.PublishDiagnosticsParams.create
          ~uri:("")
          ~diagnostics:(
            []
          )
          ()
      ))
      ()
  )
let clear_diagnostics filename = 
    Lsp_types.NotificationMessage.json_of_t (
      Lsp_types.NotificationMessage.create
        ~jsonrpc:"2.0"
        ~method_:"textDocument/publishDiagnostics"
        ~params:(Lsp_types.PublishDiagnosticsParams.json_of_t 
        (Lsp_types.PublishDiagnosticsParams.create
            ~uri:(Utils.file_str (Filepath.Normalized.of_string (Filepath.normalize filename)))
            ~diagnostics:(
              []
            )
            ()
        ))
        ()
    )

let diagnostic loc severity msg source = 
  Lsp_types.Diagnostic.create 
    ~range:(Utils.get_lsp_range loc)
    ~severity:severity
    ~message:msg
    ~source:source
    ()

let escape_double_quotes str = 
  let regex = Str.regexp {|[\"]|} in
  Str.global_replace regex {|\"|} str

let escape_unicode str = 
  let regex = Str.regexp {|(\\[0-9]+)+|} in
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
  if (Utils.contains msg ~suffix:"syntax error" 
    || Utils.contains msg ~suffix:"There were parsing errors in"
    || Utils.contains msg ~suffix:"User Error"
    || Utils.contains msg ~suffix:"invalid user input"
    || Utils.contains msg ~suffix:"Invalid symbol"
    || Utils.contains msg ~suffix:"before or at token"
  ) then 
    begin
      diag_list := (diagnostic 
        (loc)
        Lsp_types.DiagnosticSeverity.Error 
        (escape_double_quotes (escape_unicode msg))
        event.evt_plugin
        )::!diag_list;
    end
  else
  match event.evt_kind with 
  | Log.Error ->  
    Settings.Self.debug ~level:0 "Error\n%!";
    diag_list :=  (diagnostic 
        (loc)
        Lsp_types.DiagnosticSeverity.Error 
        (escape_double_quotes (escape_unicode msg))
        event.evt_plugin
        )::!diag_list
  | Log.Failure ->
      Settings.Self.debug ~level:0 "Failure\n%!";
      diag_list :=  (diagnostic 
        (loc)
        Lsp_types.DiagnosticSeverity.Error 
        (escape_double_quotes (escape_unicode msg))
        event.evt_plugin
        )::!diag_list
  | Log.Warning -> 
    Settings.Self.debug ~level:0 "Warning\n%!";
    diag_list :=  (diagnostic 
        (loc)
        Lsp_types.DiagnosticSeverity.Warning 
        (escape_double_quotes (escape_unicode msg))
        event.evt_plugin
        )::!diag_list;
(* Settings.Self.debug ~level:0 "diags handler warning : nb diags = %d\n%!" (List.length !diag_list); *)

  | Log.Result -> 
    Settings.Self.debug ~level:0 "Result\n%!";
  | Log.Debug -> 
    Settings.Self.debug ~level:0 "Debug\n%!";
    diag_list := ( (diagnostic 
        (loc)
        Lsp_types.DiagnosticSeverity.Information 
        (escape_double_quotes (escape_unicode msg))
        event.evt_plugin
        ))::!diag_list
  | Log.Feedback ->
    Settings.Self.debug ~level:0 "Feedback\n%!";
      ()

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

let handle filename : Json.json = 
  Log.add_listener (diagnostics_handler);
  Settings.Self.debug ~level:0 "listener added\n%!";

  file := filename;

  let filepath = Filepath.Normalized.of_string filename in
  let _file = File.from_filename (filepath) in 
  try
    File.init_from_c_files [_file];
      (publishDiagnostics_notification !diag_list filename)
  with
  | _exn ->
      (publishDiagnostics_notification !diag_list filename)
  

