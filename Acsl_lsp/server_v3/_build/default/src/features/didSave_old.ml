(* let file = ref ""
let plugin_sock = ref (Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0)
let error_event_handler (evt : Log.event) : unit = 
  Printf.printf "ERror event handler\n%!";
  (match evt.evt_source with 
  | Some pos -> 
    PublishDiagnostics.publish_to := (Filepath.Normalized.to_pretty_string pos.pos_path); 
  | None -> 
    PublishDiagnostics.publish_to := !file;
    Printf.printf "publish to = %s \n%!" !file;);
    PublishDiagnostics.diagnostics_handler !PublishDiagnostics.publish_to evt

let () = Log.add_listener (error_event_handler)
let () = Printf.printf "listener added\n%!"

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

  file := filename; (* note : the filename in didSave request is only used for the diagnostics *)
  Printf.printf "got filename %s \n%!" !file;

  if (List.length !PublishDiagnostics.diag_list > 0) then 
    (PublishDiagnostics.publishDiagnostics_request !PublishDiagnostics.diag_list !PublishDiagnostics.publish_to)
  else 
    (PublishDiagnostics.clear_diagnostics !PublishDiagnostics.publish_to)

(* ( try
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
); *)

   *)