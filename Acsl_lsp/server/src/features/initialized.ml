let init_folders sock = 
  try
    Utils.send_request sock (Json.save_string (Types.ResponseMessage.json_of_t (Types.ResponseMessage.create ~jsonrpc:"2.0" ~id:(Types.Int 45638715648) ~result:(`String "Initializing files...") ())));
    (* to avoid having FRAMAC_SHARE/... instead of /home/user/.opam/[version]/share/frama-c/share *)
    let framac_share = Utils.file_str Fc_config.datadir in 
    Kernel.Share.set (Fc_config.datadir);
    let share = Kernel.Share.get () in
    Filepath.add_symbolic_dir framac_share share; 

    Configuration.request_includePaths Utils.config_id sock; 
    (* todo : give proper id *)

  with Stdlib.Sys_error err -> 
    Utils.send_error_request err sock

