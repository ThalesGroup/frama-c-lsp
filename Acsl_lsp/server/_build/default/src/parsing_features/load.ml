

let load_file filename sock =
  if !States.erroring then Printf.printf "ERRORING\n%!";
  (* clear all projects *)
  Project.clear_all ();
  Configuration.set_includePaths (Configuration.(!configs));
  let file = File.from_filename (Filepath.Normalized.of_string (Filepath.normalize filename)) in

  if not !States.erroring then 
    if !States.erroring then Printf.printf "NOT ERRORING\n%!";
    let json_string = PublishDiagnostics.clear_diagnostics filename in
    Utils.send_request sock json_string;

    try
      try
        ignore (File.init_from_c_files [file]);
        States.erroring := false;

      with Log.AbortFatal msg -> (* catch abort fatal exception for acsl annot-errors because they are treated as fatal errors *)
      Printf.printf "ABORT FATAL\n%!";
      States.erroring := true;
      let json_string = PublishDiagnostics.diagnostics filename msg (Utils.dummyLoc filename) in
      Utils.send_request sock json_string;
      Errorloc.finishParsing ();
      Printf.printf "ABORT FATAL END\n%!";

    with Log.AbortError msg -> (* catch abort error exception for c syntax errors *)
      Printf.printf "ABORT ERROR\n%!";
      States.erroring := true;
      let loc = Errorloc.currentLoc () in
      let json_string = PublishDiagnostics.diagnostics filename msg loc in
      Utils.send_request sock json_string;
      Errorloc.finishParsing ();
      Printf.printf "ABORT ERROR END\n%!";
        
    
          
(* let load_file filename sock =
  (* clear all projects *)
  Project.clear_all ();
  Configuration.set_includePaths (Configuration.(!configs));
  let file = File.from_filename (Filepath.Normalized.of_string (Filepath.normalize filename)) in
  try
    Errorloc.finishParsing ();
    Project.clear_all ();
    if not (!States.erroring) then 
      let json_string = PublishDiagnostics.clear_diagnostics filename in
      Utils.send_request sock json_string;

  with Log.AbortFatal _ -> (* catch abort error in case finishParsing fails *)
    Printf.printf "ABORT FATAL\n%!";
    States.erroring := true;
    try
      ignore (File.init_from_c_files [file]);
      States.erroring := false;

    with Log.AbortError msg -> (* catch abort error in case init_from_c_files fails *)
      Printf.printf "ABORT ERROR\n%!";
      States.erroring := true;
      try
        let loc = Errorloc.currentLoc () in
        let json_string = PublishDiagnostics.diagnostics filename msg loc in
        Utils.send_request sock json_string

      with Stdlib.Invalid_argument msg -> (* catch invalid argument in case currentLoc fails *)
        Printf.printf "INVALID ARGUMENT\n%!";
        States.erroring := true;
        let json_string = PublishDiagnostics.diagnostics filename msg (dummyLoc filename) in
        Utils.send_request sock json_string; *)
      
    
