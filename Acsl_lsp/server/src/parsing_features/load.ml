(* todo : function that removes 1 to the pos_lnum field is the location returned by Errorloc.currentLoc () *)
let init_project name =
  try 
    let curr_proj = (Project.from_unique_name name) in
    Project.set_current curr_proj;
    Project.remove ();
    Project.set_current (Project.create name);
  with Project.Unknown_project ->
    Project.set_current (Project.create name)

let init_file filename sock =
  (* we create an individual project for each C file *)
  (* we remove the project if it exists *)
    Project.clear_all();
    init_project filename;

    Configuration.set_framac_options sock;
    let file = File.from_filename (Filepath.Normalized.of_string (Filepath.normalize filename)) in

    try
      ignore (File.init_from_c_files [file]);
      let json_string = PublishDiagnostics.clear_diagnostics filename in
      Printf.printf "cleared diagnostics\n%!";
      Utils.send_request sock json_string;

    with
    | Log.AbortFatal msg -> (* catch abort fatal exception for acsl annot-errors *)
      Printf.printf "ABORT FATAL\n%!";
      (try
        let json_string = PublishDiagnostics.error filename msg (Utils.dummyLoc filename) in
        Utils.send_request sock json_string;
        Errorloc.finishParsing ();
        Printf.printf "ABORT FATAL END\n%!";
      with 
      | _ -> ();
      ) (* finishParsing can cause errors but we ignore them *)

    | Log.AbortError msg -> (* catch abort error exception for c syntax errors *)
      Printf.printf "ABORT ERROR\n%!";
      (try
        let loc = Errorloc.currentLoc () in
        let json_string = PublishDiagnostics.error filename msg loc in
        Utils.send_request sock json_string;
        Errorloc.finishParsing ();
        Printf.printf "ABORT ERROR END\n%!"
      with 
        | Invalid_argument _ -> (* if currentLoc has errored we try again with dummyLoc *)
          (try
            let json_string = PublishDiagnostics.error filename msg (Utils.dummyLoc filename) in
            Utils.send_request sock json_string;
            Errorloc.finishParsing ();
            Printf.printf "ABORT ERROR END 2\n%!"
          with _ -> ();)
        | _ -> ();
        )
    (* | Log.FeatureRequest _ -> 
      Printf.printf "FEATURE REQUEST ERROR \n%!";
      (try
        let loc = Errorloc.currentLoc () in
        let json_string = PublishDiagnostics.warning filename "feature request" loc in
        Utils.send_request sock json_string;
        Errorloc.finishParsing ();
        Printf.printf "FEATURE REQUEST ERROR END\n%!"
      with 
        | Invalid_argument _ -> (* if currentLoc has errored we try again with dummyLoc *)
          (try
            let json_string = PublishDiagnostics.warning filename "feature request" (Utils.dummyLoc filename) in
            Utils.send_request sock json_string;
            Errorloc.finishParsing ();
            Printf.printf "FEATURE REQUEST ERROR END 2\n%!"
          with _ -> ();)
        | _ -> ();
        ) *)
    | _ -> 
      Printf.printf "UNKNOWN ERROR \n%!";
      (try
        let loc = Errorloc.currentLoc () in
        let json_string = PublishDiagnostics.warning filename "unknown" loc in
        Utils.send_request sock json_string;
        Errorloc.finishParsing ();
        Printf.printf "UNKNOWN ERROR END\n%!"
      with 
        | Invalid_argument _ -> (* if currentLoc has errored we try again with dummyLoc *)
          (try
            let json_string = PublishDiagnostics.warning filename "unknown" (Utils.dummyLoc filename) in
            Utils.send_request sock json_string;
            Errorloc.finishParsing ();
            Printf.printf "UNKNOWN ERROR END 2\n%!"
          with _ -> ();)
        | _ -> ();
        )

let load_files filenames sock =
  (* clear all previous projects *)
  Project.clear_all ();
  Configuration.set_framac_options sock;

  let files = List.map (fun filename -> 
      File.from_filename (Filepath.Normalized.of_string (Filepath.normalize filename));
  ) filenames in
    try
      ignore (File.init_from_c_files files);
    with 
    | Log.AbortFatal _ -> (* catch abort fatal exception for acsl annot-errors because they are treated as fatal errors *)
      Printf.printf "abort fatal \n%!"

    | Log.AbortError _ -> (* catch abort error exception for c syntax errors *)
      Printf.printf "abort error \n%!"
    | _ -> Printf.printf "Unknown error \n%!"
  
        
    
(* loads C files recursively in the workspace folder *)
let init_workspace_files sock : unit = 
  Printf.printf "WORKSPACE FILES\n%!";
  (* we create a separate project for the global AST and set it as current by default *)
    (* we remove the project if it exists *)
  init_project "global_ast";

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

let init_user_defined_files sock : unit =
  Printf.printf "USER DEFINED FILES\n%!";
  load_files !(Configuration.configs.framac_sourceFiles) sock

let init_files sock = 
  (match !(Configuration.configs.framac_sourceFiles) with
  | [] -> init_workspace_files sock
  | _ -> init_user_defined_files sock);