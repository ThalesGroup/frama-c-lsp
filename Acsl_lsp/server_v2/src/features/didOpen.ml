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

let wp_actions filename () = 
  if (Wp.Wp_parameters.WP.get ()) then
    (
      let obj = (Wp.Generator.create ()) in 
      Wp.Register.do_wp_proofs (obj#compute_main ());
      Settings.Self.debug ~level:0 "Filename : %s\n%!" filename; 
      Property_status.iter(fun prop ->
      let _property_line = (Stdlib.fst (Property.location prop)).Filepath.pos_lnum in 
      (* Settings.Self.debug ~level:0 "Property location : %s\n%!" (Pretty_utils.to_string Printer.pp_location (Property.location prop));  *)
      if (filename = (Filepath.Normalized.to_pretty_string (Stdlib.fst (Property.location prop)).Filepath.pos_path)) then
      (  ignore(Wp.VC.generate_ip prop);
        (* Settings.Self.debug ~level:0 "Property location : %s\n%!" (Pretty_utils.to_string Printer.pp_location (Property.location prop));  *)
        Settings.Self.debug ~level:0 "\tNumber of POs : %d\n%!" (List.length (Wp.VC.proof prop));)
    );
    )



let cpt = ref 0
let first_time = ref true
let filename = ref ""

let handle (req : Types.NotificationMessage.t) sock : unit = 
  ignore sock;
  let params = 
    match req.params with 
    | Some p -> Types.DidSaveTextDocumentParams.t_of_json p
    | None -> Settings.Self.debug ~level:1 "No didSave params \n%!"; assert false
  in
  filename := 
    (try 
      remove_file_scheme 
      (remove_newline 
        (remove_quotes 
          (params.textDocument.uri)
        )
      ) 
    with 
    | Invalid_argument msg -> 
      Settings.Self.debug ~level:1 "didSave: %s\n%!" msg; ""
      );
  PublishDiagnostics.publish_to := !filename; 
  let filepath = Filepath.Normalized.of_string !filename in
  let _file = File.from_filename (filepath) in 

  Settings.Self.debug ~level:0 "Projects :\n%!";
  Project.iter_on_projects (fun x ->
    Settings.Self.debug ~level:0 "\t- %s\n%!" (Project.get_name x);
  );
  
  Project.clear ();

  Settings.Self.debug ~level:0 "Current proj : %s\n%!" (Project.get_name (Project.current ()));
  Configuration.set_framac_options ();
  Settings.Self.debug ~level:0 "Processed file name : %s\n%!" (File.get_name _file);

  cpt := !cpt + 1;

( try
    File.init_from_c_files [_file];
    first_time := false;
    wp_actions (Filepath.relativize !filename) ();
    Utils.send_request sock (Json.save_string (PublishDiagnostics.publishDiagnostics_request !PublishDiagnostics.diag_list !PublishDiagnostics.publish_to));
    PublishDiagnostics.diag_list := [];
  with
  | exn ->
    Utils.send_request sock (Json.save_string (PublishDiagnostics.publishDiagnostics_request !PublishDiagnostics.diag_list !PublishDiagnostics.publish_to));
    PublishDiagnostics.diag_list := [];
    Settings.Self.debug ~level:0 "DidSave error :  %s, Backtrace : %s\n%!" (Printexc.exn_slot_name exn) (Printexc.get_backtrace ());
);

  