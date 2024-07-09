let configs = Vtypes.Configurations.create 
  ~framac_includePaths:(ref []) 
  ~framac_sourceFiles:(ref []) 
  ~framac_macros:(ref []) 
  () 

let request_includePaths client_sock : unit = 
  Printf.printf "Asking for include paths\n%!";
  let req = Types.RequestMessage.json_of_t 
  (Types.RequestMessage.create 
    ~jsonrpc:"2.0" 
    ~id:(Types.Int (Utils.config_id)) 
    ~method_:"workspace/configuration" 
    ~params:(Json.load_string 
    {|
      {
        "items":
        [
          {"section": "framac.includePaths"}
        ]
      }
    |})
    ()) in
  Utils.send_request client_sock (Json.save_string req) 

(* always update this function if configuration names change, etc. *)
let request_configurations client_sock : unit = 
  Printf.printf "Asking for configurations\n%!";
  let req = Types.RequestMessage.json_of_t 
  (Types.RequestMessage.create 
    ~jsonrpc:"2.0" 
    ~id:(Types.Int (Utils.config_id)) 
    ~method_:"workspace/configuration" 
    ~params:(Json.load_string 
    {|
      {
        "items":
        [
          {"section": "framac.includePaths"},
          {"section": "framac.sourceFiles"},
          {"section": "framac.macros"}
        ]
      }
    |})
    ()) in
  Utils.send_request client_sock (Json.save_string req)

  let validate_includePath path sock = 
    Filepath.exists (Filepath.Normalized.of_string (Utils.remove_newline (Utils.remove_quotes path))) 
    ||
    (let json_string = PublishDiagnostics.includePaths_error ("Invalid include path: "^path) in
    Utils.send_request sock json_string;
    false)

let validate_sourceFile path = 
    Filepath.exists (Filepath.Normalized.of_string (Utils.remove_newline (Utils.remove_quotes path)))
    && 
    String.ends_with ~suffix:".c" (Utils.remove_newline (Utils.remove_quotes path))


let get_framac_includePaths sock () : string list = 
  let existing_dirs = List.filter (fun path -> 
      validate_includePath path sock
  ) !(configs.framac_includePaths) in    
  List.map (fun path -> 
    (" -I"^(path))
    ) existing_dirs

let validate_macro macr =
      ignore macr; true
  (* let regex = Str.regexp {|([0-9a-zA-Z_]+(=[0-9a-zA-Z_]*)+)|} in  *)
  (* Str.string_match regex macr 0 *)
  (* todo : write proper validation function with regex *)

let get_framac_macros (): string list = 
  let macros = List.filter (fun macro -> 
      validate_macro macro
    ) !(configs.framac_macros) in    
  List.map (fun macr -> 
    (" -D"^(macr))
    ) macros
  

let set_framac_options sock : unit = 
  Kernel.CppExtraArgs.set (List.append (get_framac_includePaths sock ()) (get_framac_macros ()));
  List.iter (fun x ->
    Printf.printf "extra arg : %s\n%!" x
  ) (Kernel.CppExtraArgs.get ())


let save_configs (result: Json.json) = 
match result with
| `List [`List incl; `List src; `List macr] -> 
    configs.framac_includePaths := [];
    configs.framac_sourceFiles := [];
    configs.framac_macros := [];

    List.iter (fun x ->
      configs.framac_includePaths := ((Utils.remove_newline (Utils.remove_quotes (Json.save_string x))) :: !(configs.framac_includePaths))
    ) incl;

    (* note : List.filter ? *)
    List.iter (fun x ->
      let y = (Utils.remove_newline (Utils.remove_quotes (Json.save_string x))) in
      if validate_sourceFile y then 
        begin
          configs.framac_sourceFiles := y :: !(configs.framac_sourceFiles);
        end
        else
        configs.framac_sourceFiles := !(configs.framac_sourceFiles);
    ) src;

    List.iter (fun x ->
      configs.framac_macros := ((Utils.remove_newline (Utils.remove_quotes (Json.save_string x))) :: !(configs.framac_macros) )
    ) macr;


| _ -> Printf.printf "[acsl-lsp] Warning: Requested unknown configuration(s)."