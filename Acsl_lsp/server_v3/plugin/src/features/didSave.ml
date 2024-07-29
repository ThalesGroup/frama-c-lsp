

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
  let filepath = Filepath.Normalized.of_string filename in
  let _file = File.from_filename (filepath) in 

  (* if (List.length !PublishDiagnostics.diag_list > 0) then 
    (PublishDiagnostics.publishDiagnostics_request !PublishDiagnostics.diag_list !PublishDiagnostics.publish_to)
  else 
    (PublishDiagnostics.clear_diagnostics !PublishDiagnostics.publish_to) *)

( try
    Project.set_current (Project.create "didSave");
    File.init_from_c_files [_file];
    if (List.length !PublishDiagnostics.diag_list > 0) then 
      (PublishDiagnostics.publishDiagnostics_request !PublishDiagnostics.diag_list !PublishDiagnostics.publish_to)
    else 
      (PublishDiagnostics.clear_diagnostics !PublishDiagnostics.publish_to)
  with
  | exn ->
    Printf.printf "DidSave error :  %s, Backtrace : %s\n%!" (Printexc.exn_slot_name exn) (Printexc.get_backtrace ());
    if (List.length !PublishDiagnostics.diag_list > 0) then 
      (PublishDiagnostics.publishDiagnostics_request !PublishDiagnostics.diag_list !PublishDiagnostics.publish_to)
    else 
      (PublishDiagnostics.clear_diagnostics !PublishDiagnostics.publish_to)
);

  