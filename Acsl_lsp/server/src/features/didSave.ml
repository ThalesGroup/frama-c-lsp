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
    let json_string = PublishDiagnostics.clear_diagnostics filename in
    Utils.send_request sock json_string

  with Log.AbortError msg ->
    let loc = Errorloc.currentLoc () in
    let json_string = PublishDiagnostics.diagnostics filename msg loc in
    Utils.send_request sock json_string

  