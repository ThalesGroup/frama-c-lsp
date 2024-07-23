let init sock = 
  (* todo : give proper id *)
  (* Utils.send_request sock (Json.save_string (Types.ResponseMessage.json_of_t (Types.ResponseMessage.create ~jsonrpc:"2.0" ~id:(Types.Int 45638715648) ~result:(`String "Initializing files...") ()))); *)

  Configuration.request_configurations sock; 

  RegisterCapability.registerCapability 
    (RegisterCapability.registrationParams 
      ([RegisterCapability.registration "workspace/didChangeConfiguration"])
    ) sock;

  Log.add_listener ~kind:[Log.Feedback; Log.Warning; Log.Error; Log.Failure] (PublishDiagnostics.error_event_handler sock);



