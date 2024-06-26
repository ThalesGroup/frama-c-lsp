let handle (req : Types.NotificationMessage.t) sock : unit = 
  let req_json = (Types.NotificationMessage.json_of_t req) in
  let filename =  Utils.remove_file_scheme (Utils.remove_newline (Utils.remove_quotes (Json.save_string (Json.field "uri" (Json.field "textDocument" (Json.field "params" req_json)))))) in
  
  let files = [File.from_filename (Filepath.Normalized.of_string (Filepath.normalize filename))] in
  
  (* clear last version of the project created with the same filename *)
  let projects = Project.find_all filename in 
  List.iter (fun prj ->
    Project.set_current prj;
  (* clear AST *)
    Project.clear ();
  ) projects;

  let project = Project.create filename in 
  Project.set_current project;

  (* nb : include paths must be set for new projects *)
  Configuration.set_includePaths (Configuration.(!configs));

  try
    ignore (File.init_from_c_files files);
  with Log.AbortError msg ->
    Printf.printf "retrieved string : %s\n%!" msg;
    let pos = Log.get_current_source () in
    Utils.send_request sock (
      Json.save_string (
        Types.NotificationMessage.json_of_t (
          Types.NotificationMessage.create
            ~jsonrpc:"2.0"
            ~method_:"textDocument/publishDiagnostics"
            ~params:(Types.PublishDiagnosticsParams.json_of_t 
            (Types.PublishDiagnosticsParams.create
                ~uri:(Utils.file_str pos.pos_path)
                ~diagnostics:(
                  [
                    Types.Diagnostic.create 
                      ~range:(Utils.get_lsp_range (pos,pos))
                      ~message:msg
                      ()
                  ]
                )
                ()
            ))
            ()
          
        )
      )
    )

  