let rootPath = ref ""
let file = ref ""
let diag_list = ref []
let wrapper_port_diagnostics = 8006
let id = ref 0

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
          ~diagnostics:
            []
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
            ~diagnostics:
              []
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
  (* Printf.printf "diags handler : nb diags = %d\n%!" (List.length !diag_list); *)
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
    Printf.printf "Error\n%!";
    diag_list :=  (diagnostic 
        (loc)
        Lsp_types.DiagnosticSeverity.Error 
        (escape_double_quotes (escape_unicode msg))
        event.evt_plugin
        )::!diag_list
  | Log.Failure ->
      Printf.printf "Failure\n%!";
      diag_list :=  (diagnostic 
        (loc)
        Lsp_types.DiagnosticSeverity.Error 
        (escape_double_quotes (escape_unicode msg))
        event.evt_plugin
        )::!diag_list
  | Log.Warning -> 
    Printf.printf "Warning\n%!";
    diag_list :=  (diagnostic 
        (loc)
        Lsp_types.DiagnosticSeverity.Warning 
        (escape_double_quotes (escape_unicode msg))
        event.evt_plugin
        )::!diag_list;
(* Printf.printf "diags handler warning : nb diags = %d\n%!" (List.length !diag_list); *)

  | Log.Result -> 
    Printf.printf "Result\n%!";
  | Log.Debug -> 
    Printf.printf "Debug\n%!";
    diag_list := ( (diagnostic 
        (loc)
        Lsp_types.DiagnosticSeverity.Information 
        (escape_double_quotes (escape_unicode msg))
        event.evt_plugin
        ))::!diag_list
  | Log.Feedback ->
    Printf.printf "Feedback\n%!";
      ()

let handle filename : Json.json =
  Log.add_listener ~plugin:"wp" (diagnostics_handler);
  Printf.printf "wp listener added\n%!";

  file := filename;

  let filepath = Filepath.Normalized.of_string filename in
  let _file = File.from_filename (filepath) in 
  try
    Wp.Wp_parameters.WP.set true;
    File.init_from_c_files [_file];
      publishDiagnostics_notification !diag_list filename
  with
  | _exn ->
      publishDiagnostics_notification !diag_list filename