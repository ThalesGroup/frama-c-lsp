(* todo : function that removes 1 to the pos_lnum field is the location returned by Errorloc.currentLoc () *)
let init_project name =
  Project.iter_on_projects (fun x ->
    Settings.Self.debug ~level:1 "Project name : %s\n%!" (x.name)
  );
  try 
    (* let prj_list = (Project.find_all name) in
    List.iter (fun curr_prj ->
      Project.set_current curr_prj;
      Project.remove ();
    ) prj_list; *)
    Project.iter_on_projects (fun x ->
      if not (String.equal x.name "default") then
        (Project.set_current x;
        Project.remove ();)
      (* else 
        try
          Project.remove ()
        with Project.Cannot_remove _ ->
          let p = Project.create "default" in
          Project.set_current (p);
          try Project.remove () with Project.Cannot_remove _ -> assert false *)
    );
    Project.set_current (Project.create name);
  with Project.Unknown_project ->
    Project.set_current (Project.create name)
    (* Settings.Self.debug ~level:1 "Project created \n%!" *)

let init_file filename sock =
  (* we create an individual project for each C file *)
  (* we remove the project if it exists *)
    (* init_project filename; *)
  Project.clear_all ();

    Configuration.set_framac_options sock;
    let f = Filepath.Normalized.of_string (Filepath.normalize filename) in
    let file = File.from_filename (f) in

    try
      Settings.Self.debug ~level:1 "File : %s, filename : %s \n%!" (File.get_name file) (filename);

    with
    | Log.AbortError msg -> 
      Settings.Self.debug ~level:1 "abort error : %s\n%!" msg
    | Log.AbortFatal msg -> 
      Settings.Self.debug ~level:1 "abort fatal : %s\n%!" msg;
    | Invalid_argument msg ->
      Settings.Self.debug ~level:1 "Invalid argument : %s\n%!" msg; 
    | Assert_failure (msg, line, ch) ->
      Settings.Self.debug ~level:1 "Assert failure: %s, %d, %d \n%!" msg line ch;
      Settings.Self.debug ~level:1 "Backtrace : %s\n%!" (Printexc.get_backtrace ());
    | Failure msg ->
      Settings.Self.debug ~level:1 "Failure: %s\n%!" msg;
    | Ast.Bad_Initialization msg ->
      Settings.Self.debug ~level:1 "Bad ast initialization: %s\n%!" msg;
    (* | File_types.Bad_Initialization msg ->
      Settings.Self.debug ~level:1 "Bad ast initialization: %s\n%!" msg; *)
    | Parsing.Parse_error -> 
      Settings.Self.debug ~level:1 "Parsing error: \n%!";
    | _ as exc -> 
      Settings.Self.debug ~level:1 "Error %s, Backtrace : %s\n%!" (Printexc.exn_slot_name exc) (Printexc.get_backtrace ());
      Settings.Self.debug ~level:1 "unknown error\n%!"



let load_files filenames sock =
  init_project "global_ast";

  Configuration.set_framac_options sock;

  let files = List.map (fun filename -> 
      File.from_filename (Filepath.Normalized.of_string (Filepath.normalize filename));
  ) filenames in
    (* try 
      Errorloc.finishParsing ();
    with _ -> (); *)
    try
      File.init_from_c_files files;
    with 
    | Log.AbortError msg -> 
      Settings.Self.debug ~level:1 "abort error : %s\n%!" msg
    | Log.AbortFatal msg -> 
      Settings.Self.debug ~level:1 "abort fatal : %s\n%!" msg;
    | _ -> 
      Settings.Self.debug ~level:1 "unknown error\n%!"
      (* try 
        Errorloc.finishParsing ();
      with _ -> () *)
  
        
    
(* loads C files recursively in the workspace folder *)
let init_workspace_files sock : unit = 
  (* we create a separate project for the global AST and set it as current by default *)
    (* we remove the project if it exists *)

  let c_files = ref [] in
  let rec init_files_rec rootPath = 
    let curr_files = ref [] in 
    (* read all files and folders of current directory *)
    let filenames = Array.to_list (Filepath.readdir (Filepath.Normalized.of_string rootPath)) in
    (* make paths absolute *)
    curr_files := List.append !curr_files (List.map(fun x ->
      rootPath^"/"^x
    ) filenames);
    (* remove non source files *)
    c_files := List.append !c_files (List.filter (fun x -> String.ends_with ~suffix:".c" x) (!curr_files));
    (* call the function recursively if folders were found in the current directory *)
    let folders = List.filter (fun x -> Sys.is_directory x) !curr_files in
    List.iter (fun folder ->
      init_files_rec (folder)
    ) folders;
  in
  init_files_rec !States.rootPath;
  load_files !c_files sock

let init_user_defined_files files sock : unit =
  load_files files sock

let init_files sock = 
  ignore sock;
  Configuration.set_framac_options sock;
  let f = Filepath.Normalized.of_string (Filepath.normalize "/home/user/git/L1/T0304764/acsl_lsp/Acsl_lsp/server/test_files/test1.c") in
  let file = File.from_filename (f) in
  if not (Ast.is_computed ()) then 
    File.init_from_c_files [file];
  (* match !(Configuration.sourceFiles) with
  | [] -> init_workspace_files sock
  | list -> init_user_defined_files list sock; *)